package net.snowtiger.spliced.search

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition.{Composition, MusicCount}
import net.snowtiger.spliced.tables.Node.leadBitSet
import net.snowtiger.spliced.tables.{EmptyNode, Node, Tables}

import scala.collection.BitSet
import scala.collection.immutable.TreeSet
import scala.util.Random


/**
 * @author mark
 */

abstract class SearchBase(val searchDef: SearchDefinition)
{
	val out = searchDef.logger
	val compPlan = searchDef.getCompPlan
	val tables = new Tables(searchDef)

	var highestMusic = new MusicCount(compPlan.musicDefs)
	var highestMusicComp: Composition = null
	var bestComps: TreeSet[Composition] = null

	def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])

	final def varyFromSeed() =
	{
		out.println("Calling: "+compPlan.compStr)
		out.println("Methods: "+searchDef.methods.map{_.name}.mkString(", "))
		bestComps = buildTables()
		val t = System.currentTimeMillis()
		varyCompsUntilScoreMaximized(new TreeSet[Composition]() ++ bestComps)
		val variations = bestComps
		val (atwVariations, nonAtw) = variations.partition(_.isAtw)
		out.println("Calling: "+compPlan.compStr)
		out.println(atwVariations.size + " ATW variations:")
		atwVariations.foreach( out.println )
		out.println(nonAtw.size + " non-ATW variations:")
		nonAtw.foreach( out.println )
		out.println("Highest music was:")
		out.println(highestMusicComp)
		out.println
		outputSearchHistory()
		out.println(" Search time "+(System.currentTimeMillis()-t)/60000+"mins")
		variations
	}

	def outputSearchHistory() {}

	/** Returns set of seed compositions */
	def buildTables(): TreeSet[Composition] =
	{
		out.println("Build tables for composition plan "+compPlan.shortDesc)
		if (compPlan.partend==Row(tables.nbells))
		{
			if (compPlan.nparts>1)
			{
				out.println("Not a multipart: rounds at first part end")
				System.exit(0)
			}
		}
		else
		{
			if (compPlan.nparts==1)
			{
				out.println("Does not end in rounds: "+compPlan.partend)
				System.exit(0)
			}
		}
		tables.buildPrepTables()
		out.println("Found "+compPlan.nodeTypeTable.getAll.size+" node types and "+compPlan.nodeTypeTable.getAll.map{tables.splices(_).size}.sum+" total callings")
		// We build the seed compositions first, before building the full set of all possible nodes.
		// This gives us the option to exclude certain types of node from the full build - for example, if we're only
		// wanting to vary one or two methods, we can fix the other methods.
		var comps = searchDef.seedProvider.getSeeds(this)
		tables.buildNodeFalseness()
		out.println()
		if (comps.size==1 && comps.head.nodes.exists{!_.isInTable})
		{
			val comp = comps.head
			val (good, bad) = comp.nodes.partition{_.isInTable}
			var trueComps = remakeFalseComp(comp, good, bad, true)
			if (trueComps.size==0)
			{
				if (false)
				{
					out.println("Cannot remake true comp: " + bad)
					System.exit(0)
				}
				trueComps = List(nonStrictRemakeFalseComp(comp, good, bad))
			}
			comps = TreeSet[Composition]() ++ trueComps
		}
		//tables.analyseNodes(comps.head)
		comps
	}

	val allowEmptyNodes = false
	val emptyNodeRatio = 8

	/**
	 * It is much quicker to randomly pick a node from the alternatives list, and see whether it is true,
	 * retrying with other random node alternatives if not, than it is to filter out all the true alternatives
	 * and then pick a random node from that smaller list.
	 * <p>
	 * It is possible that no true alternative can be found. We try up N/3 times, where N is the total size of the
	 * alternatives array. If all attempts fail, we will pick another random part of the composition, and repeat
	 * the whole process up to {@code triesLeft} times. If we have absolutely no luck finding a true alternative
	 * node, we will attempt to use a false node and force the rest of the composition to be true. Finally,
	 * if this also fails, we return {@code None}.
	 * <p>
	 * Note that this routine has been tuned for performance in other ways; for instance, we assume that the
	 * {@code nodeAlternatives} are already built as arrays - much quicker to index into than if they were sets to start.
	 * Various code optimisations over 10-12 December 2013 have given two orders of magnitude performance improvement -
	 * from about 500 nodes/second to 60,000.
	 *
	 * @param comp
	 * @param triesLeft
	 * @return
	 */
	def randomMove(comp: Composition, triesLeft: Int): Option[Composition] =
	{
		val pos = getRandomPos(false)
		val nodeToVary = comp.getNodeAt(pos)
		if (pos>0 && allowEmptyNodes && Random.nextInt(emptyNodeRatio)==0)
			Some(comp.replaceNode(pos, EmptyNode(nodeToVary)))
		else
			randomNodeChange(pos, nodeToVary, comp, triesLeft)
	}

	private def getRandomPos(scaleByAltSize: Boolean): Int =
	{
		if (!scaleByAltSize)
			Random.nextInt(compPlan.calling.size)
		else
		{
			val r = Random.nextInt(tables.totalNodeAlts)
			var (pos, a, b) = tables.cumulativeAltSizes.find{ (t) => r>=t._2 && r<t._3 }.get
			pos
		}
	}

	private def randomNodeChange(pos: Int, nodeToVary: Node, comp: Composition, triesLeft: Int): Option[Composition] =
	{
		val falseLeads = comp.falseLeadBitSetWithoutNode(pos)
		//val foundAlt = standardRandomNodeChange(pos, nodeToVary, comp, falseLeads)
		val foundAlt = nodeTreeRandomNodeChange(pos, nodeToVary, comp, falseLeads)
		if (foundAlt.isDefined)
		{
			Some(comp.replaceNode(pos, foundAlt.get))
		}
		else if (triesLeft <= 0)
		{
			// Try to punch through false valley if no true alternatives for this node exist
			val alternatives = tables.nodeAlternatives(nodeToVary.startLH)
			val j = Random.nextInt(alternatives.size)
			makeTrueBest(comp, pos, alternatives(j), 2)
		}
		else
		{
			randomMove(comp, triesLeft - 1)
		}
	}

	private def standardRandomNodeChange(pos: Int, nodeToVary: Node, comp: Composition, falseLeads: BitSet): Option[Node] =
	{
		val alternatives = tables.nodeAlternatives(nodeToVary.startLH)
		var foundAlt: Option[Node] = None
		val nAlts = alternatives.size
		var n = 1.max(nAlts/3)
		// Pick a random index just once, and simply decrement this if the first choice proves false
		var j = Random.nextInt(nAlts)
		while (n >= 0 && foundAlt.isEmpty)
		{
			n -= 1
			val alt = alternatives(j)
			if (alt.n != nodeToVary.n && alt.notFalseAgainst(falseLeads))
				foundAlt = Some(alt)
			else
				j = if (j==0) nAlts-1 else j-1
		}
		foundAlt
	}

	private def nodeTreeRandomNodeChange(pos: Int, nodeToVary: Node, comp: Composition, falseLeads: BitSet): Option[Node] =
	{
		val alternatives = tables.nodeAlternatives(nodeToVary.startLH)
		var foundAlt: Option[Node] = None
		val nAlts = alternatives.size
		var n = 1.max(nAlts/50)
		while (n >= 0 && foundAlt.isEmpty)
		{
			n-= 1
			foundAlt = tables.nodeAltTrees(nodeToVary.startLH).findRandomTrue1(falseLeads)
		}
		foundAlt
	}

	def randomMoveAllowFalse(comp: Composition, triesLeft: Int): Option[Composition] =
	{
		val pos = Random.nextInt(comp.nodes.size)
		val nodeToVary = comp.getNodeAt(pos)
		val alternatives = tables.nodeAlternatives(nodeToVary.startLH)
		val nAlts = alternatives.size
		var n = 2
		var foundAlt: Option[Node] = None
		while (n >= 0 && foundAlt.isEmpty)
		{
			n -= 1
			val j = Random.nextInt(nAlts)
			val alt = alternatives(j)
			if (alt.n != nodeToVary.n)
				foundAlt = Some(alt)
		}
		if (foundAlt.isDefined)
			Some(comp.replaceNode(pos, foundAlt.get))
		else
			None
	}

	def keepComps(newComps: List[Composition], variations: TreeSet[Composition], max: Int): TreeSet[Composition] =
	{
		var newVariations = variations;
		for (comp <- newComps)
			newVariations = keepComp(newComps.head, newVariations, max)
		newVariations
	}

	def keepComp(comp: Composition, variations: TreeSet[Composition], max: Int): TreeSet[Composition] =
	{
		keepBest(comp)
		doKeepComp(comp, variations, max)
	}

	val nCompsToKeep = 50

	def keepBest(comp: Composition)
	{
		bestComps = doKeepComp(comp, bestComps, nCompsToKeep)
		if (comp.music>highestMusic)
		{
			highestMusic = comp.music
			highestMusicComp = comp.toImmutableComp
		}
		else if (comp.music==highestMusic)
		{
			if (comp.score>highestMusicComp.score)
				highestMusicComp = comp.toImmutableComp
		}
	}

	def doKeepComp(comp: Composition, variations: TreeSet[Composition], max: Int): TreeSet[Composition] =
	{
		val MIN_DIST = 50
		// The Levenshtein distance calculations really slow down the search!
		def calcMinDist = 100 //variations.map{comp.differenceBetween(_)}.min
		if (variations.size<max)
			variations + comp.toImmutableComp
		else if (comp.score<=variations.lastKey.score)
			variations
		else if (comp.score<variations.firstKey.score && calcMinDist<MIN_DIST)
			variations
		else
			variations - variations.lastKey + comp.toImmutableComp
	}

	/** If the touch is false, try to vary all the false nodes to make it true */
	def forceTouchTrue(comp: Composition): List[Composition] =
	{
		val allNodes = comp.nodes
		val allNodesSet = allNodes.toSet
		def markTrue(node: Node) = (node, node.trueWith(allNodesSet-node))
		val partitioned = allNodes.map{markTrue(_)}.partition(_._2)
		val trueNodes = partitioned._1.map(_._1)
		val falseNodes = partitioned._2.map(_._1)
		if (falseNodes.isEmpty)
			List(comp)
		else
		{
			out.println("False comp "+comp)
			remakeFalseComp(comp, trueNodes, falseNodes, true)
		}
	}

	/** Given a single false node, try to fix the rest of the composition to make it true; return the best of the resulting compositions, if any */
	def makeTrueBest(wholeFalseComp: Composition, newNodePos: Int, newNode: Node, maxFalse: Int): Option[Composition] =
	{
		val (pre, post) = wholeFalseComp.nodes.splitAt(newNodePos)
		val trueComps = makeTrue(wholeFalseComp, pre, newNode, post.tail, maxFalse)
		if (trueComps.isEmpty)
		{
			// Couldn't process because too many false nodes
			None
		}
		else
		{
			var best:Option[Composition] = None
			var bestScore = -1
			for (comp <- trueComps.get)
				if (bestScore < comp.score)
				{
					best = Some(comp)
					bestScore = comp.score
				}
			best
		}
	}

	/** Given a single false node, try to fix the rest of the composition to make it true. Returns None if too many false nodes to process, else list (which may be empty). */
	def makeTrue(wholeFalseComp: Composition, pre: List[Node], newNode: Node, post: List[Node], maxFalse: Int): Option[List[Composition]] =
	{
		val (preTrue, preFalse) = partitionByTruth(newNode, pre)
		val (postTrue, postFalse) = partitionByTruth(newNode, post)
		val nFalseNodes = preFalse.size+postFalse.size
		if (nFalseNodes > maxFalse)
			None
		else
		{
			val trueNodes = preTrue ++ (newNode::postTrue)
			val falseNodes = preFalse++postFalse
			Some(remakeFalseComp(wholeFalseComp, trueNodes, falseNodes, true))
		}
	}

	/**
	 * Given a false composition, with a maximal subset {@code trueNodes} of mutually true nodes,
	 * attempt to replace the remaining nodes {@code falseNodes} with nodes that are true both
	 * against the true set and each other; if possible then reassemble into one or more true
	 * compositions.
	 */
	def remakeFalseComp(wholeFalseComp: Composition, trueNodes: List[Node], falseNodes: List[Node], justOne: Boolean) =
	{
		val leadBits = leadBitSet(trueNodes)
		// Find the set of alternative nodes for each false node.
		// In each set, all alternatives must be true against the original trueNodes set of the composition
		val alts = falseNodes.map{ (n)=> tables.nodeAlternatives(n.startLH).filter{_.trueWith(leadBits)} }
		// If any set is empty, there is no way of making the original composition true (without altering the trueNodes anyway)
		if (alts.exists{_.isEmpty})
			Nil
		else
		{
			// Find true replacement sequences, where a sequence is one node from each alternative node set
			val revReplacementSeqs = if (justOne)
				randomFindOneTrueNodeSequence(alts).toList
			else
				findTrueNodeSequences(alts.head.toList.map{ _::Nil }, alts.tail)
			// Each replacement sequence can be reassembled into a true composition
			revReplacementSeqs.map{ (revSeq)=> reassembleComp(Nil, wholeFalseComp, 0, trueNodes, revSeq.reverse) }
		}
	}

	/**
	 * Given a false composition, with a maximal subset {@code trueNodes} of mutually true nodes,
	 * replace the remaining nodes {@code falseNodes} with nodes that are at least in the composition
	 * (but may well be false against each other or the rest of the composition - so the search will
	 * start in a false state).
	 */
	def nonStrictRemakeFalseComp(wholeFalseComp: Composition, trueNodes: List[Node], falseNodes: List[Node]) =
	{
		// Just take the first alternative nodes for each false node.
		val alts = falseNodes.map{ (n)=> tables.nodeAlternatives(n.startLH).head}
		reassembleComp(Nil, wholeFalseComp, 0, trueNodes, alts)
	}

	def partitionByTruth(node: Node, nodes: List[Node]) = nodes.partition{ (n)=> node.trueWith(n.leads) }

	/** Turn a list of sets of alternative nodes into a list of true node sequences, one node from each set.
		* We expect the alternative node sets to be non-empty!
		* Note this routine can produce very large numbers of compositions, and hence use lots of memory and run for a long time. */
	def findTrueNodeSequences(revTrueSeqs: List[List[Node]], alts: List[Array[Node]]): List[List[Node]] =
	{
		if (alts.isEmpty)
			revTrueSeqs
		else
		{
			var extendedSeqs = List[List[Node]]()
			for (node <- alts.head; seq <- revTrueSeqs)
				if (node.trueWith(seq))
					extendedSeqs::= node::seq
			if (extendedSeqs.isEmpty)
				Nil
			else
				findTrueNodeSequences(extendedSeqs, alts.tail)
		}
	}

	/** Attempts to find a single true node sequence from the given list of alternatives. */
	def randomFindOneTrueNodeSequence(altsList: List[Array[Node]]): Option[List[Node]] =
	{
		var revTrueSeq = List[Node]()
		val altIt = altsList.iterator
		var canContinue = true
		while (canContinue && altIt.hasNext)
		{
			val alts = altIt.next()
			val nAlts = alts.size
			var n = nAlts/3
			var foundAlt: Option[Node] = None
			var leadBits = leadBitSet(revTrueSeq)
			while (n>=0 && foundAlt.isEmpty)
			{
				n-= 1
				val j = Random.nextInt(nAlts)
				val alt = alts(j)
				if (alt.trueWith(leadBits))
					foundAlt = Some(alt)
			}
			if (foundAlt.isDefined)
				revTrueSeq = foundAlt.get::revTrueSeq
			else
				canContinue = false
		}
		if (canContinue)
			Some(revTrueSeq)
		else
			None
	}

	/**
	 * Reassemble a composition from two lists of nodes. The original composition must be provided.
	 * The two lists of nodes must be in order, and their leadheads must match those of the original composition;
	 * they are interleaved to reform the composition.
	 */
	def reassembleComp(revReassembled: List[Node], originalComp: Composition, i: Int, unaffectedNodes: List[Node], replacementNodes: List[Node]): Composition =
	{
		if (i==originalComp.nodes.length)
			originalComp.makeNew(revReassembled.reverse, tables)
		else if (!unaffectedNodes.isEmpty && originalComp.nodes(i).startLH==unaffectedNodes.head.startLH)
			reassembleComp(unaffectedNodes.head::revReassembled, originalComp, i+1, unaffectedNodes.tail, replacementNodes)
		else
			reassembleComp(replacementNodes.head::revReassembled, originalComp, i+1, unaffectedNodes, replacementNodes.tail)
	}

}

