package net.snowtiger.spliced.tables

import net.snowtiger.ringing.{Method, NamedMethod, Row}
import net.snowtiger.spliced.composition.{Call, NodeType, _}
import net.snowtiger.spliced.search.SearchDefinition

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer
import scala.collection.{BitSet, SortedMap, mutable}
import scala.util.Random

/**
 * @author mark
 */

class Tables(searchDef: SearchDefinition)
{
	val methods = searchDef.methods
	val compPlan = searchDef.getCompPlan
	val out = searchDef.logger
	def scoreFn(comp: Composition) = searchDef.scoreFn(comp)

	assert(!methods.isEmpty)

	// Ensure all methods have the same leadhead group. If this is not true, our lead table population won't work, since
	// we only generate plain courses of each method from each node head. Also of course splices could get very long.
	for (method <- methods.tail)
		if (false)
		assert(method.leadHeads.toSet==methods.head.leadHeads.toSet, "Methods have different LH group: "+method+", "+methods.head)

	val method1 = compPlan.singleMethod
	val nbells = method1.nbells

	// Node and Lead tables (NodeType table is in CompositionPlan)
	val nodeTable = new NodeTable()
	val leadTable = new LeadTable()

	// Temporary tables used whilst building lead falseness and nodes
	val splices = mutable.Map[NodeType, TreeSet[Splice]]()
	val leadRows = mutable.Map[Lead, Set[Row]]()
	val crossFalseness = mutable.Map[Set[NamedMethod], Int]()
	val methodLeadCounts = mutable.Map[NamedMethod, Int]()
	val nodeLeads = mutable.Map[Int, List[Int]]()
	val altCombinedLeadSets = mutable.Map[Row, BitSet]()

	// For each node, lists the alternatives available for inclusion in the comp (via the starting LH).
	var nodeAlternatives: Map[Row, Array[Node]] = _
	private val nodeAlternativesWorking = mutable.Map[Row, Set[Node]]()
	var nodeAltTrees: Map[Row, NodeTree] = _
	var totalNodeAlts = 0
	var cumulativeAltSizes = List[(Int,Int,Int)]()

	/**
	 * Builds the splice and lead-false tables - must be done before instantiating any Nodes or Compositions.
	 */
	def buildPrepTables()
	{
		out.println("Building lead falseness tables")
		compPlan.addExcludedLeads(searchDef.getExcludedLeads, leadTable)
		time("Lead false build", buildLeadFalse())
		time("Splice build", buildSplices())
	}

	/**
	 * A Splice is a set of plain leads in different methods between two calling positions (the last lead is always
	 * bobbed, except potentially at the part end).
	 * Any number of leads between 1 and 7 (for Major) is possible, depending on the methods. Note that,
	 * with more than 4 or 5 methods to choose from, depending on falseness, there can be tens of thousands of
	 * possibilities for each splice, and this method processes splices between all possible pairs of calling positions:
	 * for tenors-together Major with bobs only, there are sixteen types (H->H, H->W, H->B, H->M, W->H, W->W, etc).
	 *
	 * @return
	 */
	private def buildSplices()
	{
		def nextLeadHeadByCall(lh: Row, m: Method, call: Call) = lh.apply(m.callPerm(call.pn))

		var nodeSplices:TreeSet[Splice] = null

		/** Recursive call builds all splices between start and end calling positions of the node type,
			* using (for the time being) the plain course */
		def buildSplices(nodeType: NodeType, currentLH: Row, leadheads: Set[Row], splice: List[NamedMethod])
		{
			def addSplice(splice: Splice)
			{
				// Allow search definition to exclude splices from the search space. By default all splices are included.
				if (searchDef.acceptSplice(splice))
					nodeSplices+= splice
			}

			// Only check falseness against leadheads here.
			// We'll check the internal falseness later, when we attach the splice to individual nodes.
			// This is essential for multiparts - there is different falseness between parts dependent on the actual start LH.
			if (!leadheads.contains(currentLH))
				for (m <- methods)
				{
					val tryLead = Lead(currentLH, m)
					if (searchDef.acceptLead(tryLead))
					{
						val lead = leadTable.getInterned(tryLead)
						val nextLH = currentLH.apply(m.plainPerm)
						// Allowed to finish on a plain lead if nodeType is plained
						if (nextLH==nodeType.end.plainCourseLH && nodeType.endCall.isPlain)
							addSplice(Splice((m::splice).reverse, compPlan))
						else
						{
							if (!nodeType.endCall.isPlain &&
									nodeType.end.maskedLH==nodeType.end.maskAllButTenor(nextLeadHeadByCall(currentLH, m, nodeType.endCall)))
								addSplice(Splice((m::splice).reverse, compPlan))
							// Worth continuing to look at other splices since there may be another way to bring up the call
							buildSplices(nodeType, nextLH, leadheads+currentLH, m::splice)
						}
					}
				}
		}

		var progress = new TableBuildProgress(1, out)
		for (nodeType <- compPlan.nodeTypes)
		{
			nodeSplices = TreeSet()
			buildSplices(nodeType, nodeType.start.plainCourseLH, Set(), Nil)
			splices+= nodeType->nodeSplices
			progress.emit
		}
		out.println()
	}

	/**
	 * Builds the lead-falseness tables. Only leads which can be in the seed composition are generated - i.e.
	 * the plain course of every method from each node head. If the seed composition ends in a part end other
	 * than rounds, then the each lead will be populated with all the rows for every part.
	 */
	private def buildLeadFalse()
	{
		var progress = new TableBuildProgress(10, out)

		def genPlainCourse(startLH: Row, revLHs: List[Row], method: NamedMethod): List[Lead] =
		{
			var nextLH = revLHs.head.apply(method.plainPerm)
			if (nextLH==startLH)
				revLHs.reverse.map{ (lh)=> leadTable.intern(Lead(lh, method)) }
			else
				genPlainCourse(startLH, nextLH::revLHs, method)
		}

		for (compNodeHead <- compPlan.nodeStartLeads; method <- methods)
			genPlainCourse(compNodeHead, List(compNodeHead), method).foreach{ (l)=> addLead(l); progress.emit }
		// Must also add the excluded leads, in order to generate cross-falseness against the desired leads
		for (lead <- compPlan.excludedLeads)
		{
			addLead(lead);
			compPlan.excludedMusic = compPlan.excludedMusic + lead.music
			progress.emit
		}
		out.println()
		out.println(leadRows.size+" leads")

		out.println("Method cross falseness:")
		crossFalseness.foreach( (p)=>
		{
			val crossSet = p._1
			val falseCount = p._2.toDouble
			val total = crossSet.toList.map( methodLeadCounts(_) ).reduceLeft( (a,b)=>a*b )
			out.println(crossSet.mkString(",")+": "+(falseCount/total))
		})

		// Clear temporary
		leadRows.clear()
		crossFalseness.clear()
		methodLeadCounts.clear()
	}

	def addLead(lead: Lead)
	{
		if (!leadRows.contains(lead))
		{
			// Generate rows for lead for all parts, and calculate music
			lead.music = new MusicCount(compPlan.musicDefs)
			var rowsPerPart = List[List[Row]]();
			for (lh <- compPlan.permuteByPartEnds(lead.startLH))
			{
				val buf = ListBuffer[Row]()
				lead.method.generateLead(lh, buf)
				var rowList = buf.toList
				// Truncate leads which have internal rounds! But record this because we can't use them in the middle of a comp
				val rowsUpToRounds = rowList.head::(rowList.tail.takeWhile{_!=Row(nbells)} )
				if (rowsUpToRounds.size<rowList.size)
					lead.containsRounds = true
				rowsPerPart = rowsUpToRounds::rowsPerPart
				val music = new MusicCount(compPlan.musicDefs).count(buf)
				if (music(0)==0)
					lead.nleadsWithoutMusic+= 1
				lead.music+= music
			}
			val rowList = rowsPerPart.flatten
			val rows = rowList.toSet
			if (rowList.size!=rows.size)
			{
				//out.println("Lead is internally false in this multipart: "+lead)
				lead.internallyFalse = true
			}
			else
			{
				// The falseLeads table always includes the lead itself
				lead.falseLeads+= lead.n
				// Compare against all previously-generated leads to build falseness sets
				for (other <- leadRows.keySet)
				{
					if (!trueSets(rows, leadRows(other)))
					{
						lead.falseLeads+= other.n
						other.falseLeads+= lead.n
						val crossSet = Set(lead.method, other.method)
						crossFalseness+= crossSet -> (1+crossFalseness.getOrElse(crossSet, 0))
					}
				}
				// Finally add the lead's rows to leadRows, for falseness checks with subsequent leads
				leadRows+= lead->rows
				methodLeadCounts+= lead.method -> (1+methodLeadCounts.getOrElse(lead.method, 0))
			}
		}
	}


	// --------------------------------------------------------------------------------------------------

	/**
	 * This is the main node-falseness build.
	 */
	def buildNodeFalseness()
	{
		def generateAllNodes()
		{
			var progress = new TableBuildProgress(1000, out)
			for (compNode <- compPlan.calling; splices <- splices(compNode.nodeType))
			{
				val node = nodeTable.intern( NodeImpl(compNode.startLH, compNode.nodeType, splices) )
				if (!node.isInTable)
					addToTable(node)
				progress.emit
			}
			out.println()
			out.println(nodeTable.size+" nodes included in table")
		}

		time("Node build", generateAllNodes())
		// Convert the alternative node sets into arrays instead - much more efficient to traverse and index during the search
		// Also randomise node lists as we copy over
		//def altArrays(a: Tuple2[Row, Set[Node]]) = a._1 -> randomUnSort(a._2.toArray)
		def altArrays(a: Tuple2[Row, Set[Node]]) = a._1 -> a._2.toArray
		nodeAlternatives = nodeAlternativesWorking.map{altArrays}.toMap
		// Prune out nodes completely false against a given plan position.
		time("Node prune", pruneNodes())
		// Form node trees
		def makeNodeTree(lh: Row) = new NodeTree(nodeAlternatives(lh).map{(node)=> (node, nodeLeads(node.n))}.toList)
		nodeAltTrees = Map[Row,NodeTree]() ++ compPlan.calling.map{(n)=> (n.startLH, makeNodeTree(n.startLH))}

		var n = 0
		var i = 0
		def altSize(alts: Seq[Node]) = Math.max(1, Math.sqrt(alts.size).toInt)
		cumulativeAltSizes = nodeAlternatives.values.map{(alts) => {val size = altSize(alts); val res=(i, n, n+size); i+=1; n+=size; res} }.toList
		totalNodeAlts = n

		nodeAlternativesWorking.clear()
		splices.clear()
		nodeLeads.clear()
	}

	def randomUnSort[T](a: Array[T]) =
	{
		def swap(i: Int, j: Int) = { val t = a(i); a(i) = a(j); a(j)=t}
		def random(i: Int)
		{
			val nLeft = a.length-i
			if (nLeft>1)
			{
				swap(i, i+Random.nextInt(nLeft))
				random(i+1)
			}
		}
		random(0)
		a
	}

	/** Generates the leads in the node, finds the false-lead bitset, and checks internal falseness;
		* if all good, calculate music, add to nodeAlternatives, and adds to the table, otherwise does not.
		* Returns the list of leads added - this can be useful if you want to force the node into the table
		* despite falseness (e.g. it is part of the seed comp). */
	def addToTable(node: Node): List[Lead] =
	{
		val leads = node.genLeads(leadTable)

		// First check internal node leads do not collide with the start LH of any other node - this can happen if the seed
		// composition visits the same course twice. Want to exclude inevitable collisions.
		// TODO could do better by combining nodes which visit the same course, i.e. treat them as one node (but would need to ensure entry/exit points present)
		// Note there can still be inevitable internal falseness between nodes, which would render this one unusable;
		// this can be checked and pruned for in a separate phase (pruneNodes).
		val seedNode = inSeed(node)
		if (seedNode || leads.tail.forall{ (l:Lead)=> !compPlan.nodeStartLeads(l.startLH) } )
		{
			// Check node is not internally false. In doing this we build the falseleads bitset as a side effect:
			// this is the union of the false leads of all the leads in this node (which include the leads themselves)
			// Accumulating and storing it just once per node is much more efficient than doing it in the inner loop.
			// Note that the internally-false check works in the loop only because the node falseleads don't yet include
			// the lead itself!
			node.falseLeads = BitSet()
			def isInternallyFalse(lead: Lead) = {val isFalse = lead.internallyFalse || lead.containsRounds || node.falseLeads.contains(lead.n); node.falseLeads++=lead.falseLeads; isFalse}
			// We have to use forall instead of exists because we need to process every lead!!
			val isInternallyTrue = leads.forall(!isInternallyFalse(_))
			node.internallyFalse = if (isInternallyTrue) 0 else 1
			if (seedNode || (isInternallyTrue && (node.falseLeads & compPlan.excludedLeadNumbers).isEmpty))
				doAddNode(node, leads)
		}
		leads
	}

	def doAddNode(node: Node, leads: List[Lead]): Unit =
	{
		// Calc music
		node.music = MusicCount.sumAll(leads.map{	_.music	})
		node.leadsWithoutMusic = leads.map{_.nleadsWithoutMusic}.sum
		// Allow search definition to reject nodes - e.g. too long with too little music
		if (searchDef.acceptNode(node, leads))
		{
			// Add to nodeAlternatives table
			val startLH = node.startLH
			val altSet = nodeAlternativesWorking.getOrElse(startLH, Set())
			nodeAlternativesWorking += startLH -> (altSet + node)
			// Add the list of leadnums to the nodeLeads table (temporary, used in node pruning phase)
			val leadNums = leads.map{_.n}
			nodeLeads += node.n -> leadNums
			// Calc permanent node properties - just the leads bitset now, ATW and MethodUseTracker built lazily.
			node.leads = BitSet() ++ leadNums
			node.isInTable = true
		}
	}

	def analyseNodes(seedComp: Composition)
	{
		out.println()
		for (compNode <- seedComp.nodes)
		{
			var bestAlts = SortedMap[Int,List[Node]]()
			for (node <- nodeAlternatives(compNode.startLH))
			{
				val nFalse = node.falseLeads.size
				bestAlts.get(nFalse) match
				{
					case None => bestAlts+= nFalse->List(node)
					case Some(other) =>
						if (other.head.music(0)<node.music(0))	bestAlts+= nFalse->List(node)
						else if (other.head.music(0)==node.music(0)) bestAlts+= nFalse->(node::other)
				}
			}
			val it = bestAlts.iterator
			var best = it.next()
			while (best._2.head.music(0)<compNode.music(0))
				best = it.next()
			out.println(compNode+" "+compNode.falseLeads.size+" lowest falseness "+best)
		}
	}

	def populateCombinedLeadSets()
	{
		for (compNode <- compPlan.calling)
			altCombinedLeadSets+= compNode.startLH -> Node.leadBitSet(nodeAlternatives(compNode.startLH))
	}

	/**
	 * Identify and remove any nodes which are false with all the alternative nodes in at least one position of the
	 * composition; in other words, nodes which can never be used. There are surprisingly many of these in most
	 * composition plans. After the first pruning pass it may be that, because there are fewer alternatives in a given
	 * position, there are now new nodes elsewhere which are false with that position; hence we repeat the pruning
	 * operation until no more nodes are pruned.
	 * <p>
	 * Note that this algorithm was impracticably slow to start with: a typical composition plan might have a total of
	 * one million nodes, so in theory 1 trillion operations to check them all against each other. However by grouping
	 * the alt-nodes within a position into a tree based on the leads making up each node (see {@link LeadTree}), is
	 * it possible to do this much more efficiently: this is helped by the feature that, if a node is going to be false
	 * against another, it is quite often false in the first lead.
	 */
	def pruneNodes()
	{
		var totalNodesPruned = 0
		var stillPruning = true
		while (stillPruning)
		{
			val nPruned = pruneNodesIteration()
			totalNodesPruned+= nPruned
			if (nPruned==0) stillPruning=false
		}
		out.println("Total of "+totalNodesPruned+" nodes removed, "+nodeAlternatives.values.map{_.size}.sum+" remaining")
		assert(nodeAlternatives.values.forall(!_.isEmpty), "No possible alts for nodes "+nodeAlternatives.filter{_._2.isEmpty}.map{_._1})
	}

	val callingMethodSet = Set(searchDef.getCallingMethod)
	def inSeed(node: Node) = node.methodsUsed==callingMethodSet

	def pruneNodesIteration() =
	{
		// Form tree of leads for each node type - must redo this for each iteration, as altNode lists get smaller
		def makeLeadTree(lh: Row) = new LeadTree(nodeAlternatives(lh).map{(node)=> nodeLeads(node.n)}.toList)
		val leadTrees = Map[Row,LeadTree]() ++ compPlan.calling.map{(n)=> (n.startLH, makeLeadTree(n.startLH))}

		def isInevitablyFalse(node: Node, progress: TableBuildProgress) =
		{
			def canBeTrue(lh: Row) = lh==node.startLH || leadTrees(lh).canBeTrue(node)
			def foundBad = compPlan.calling.exists{(n)=> ! canBeTrue(n.startLH)}
			progress.emit
			/*!inSeed(node) &&*/ foundBad
		}

		var totalNodesPruned = 0
		for (compNode <- compPlan.calling)
		{
			var progress = new TableBuildProgress(1000, out)
			//out.print(compNode+": ")
			val lh = compNode.startLH
			var alts = nodeAlternatives(lh)
			var trueAlts = alts.filter{!isInevitablyFalse(_, progress)}
			val nRemoved = alts.size-trueAlts.size
			totalNodesPruned+= nRemoved
			if (nRemoved>0)
				nodeAlternatives+= lh->trueAlts
			//out.println(" "+nRemoved+" nodes pruned, "+trueAlts.size+" remaining.")
		}
		out.println(totalNodesPruned+" nodes pruned in this iteration")
		totalNodesPruned
	}

	def rankNode(node: Node)
	{
		if (node.music(0)==0)
			node.setRank(0.0)
		else
		{
			if (true)
			{
				val nPosCompletelyTrue = compPlan.calling.count{(n)=> node.trueWith(altCombinedLeadSets(n.startLH))}
				node.setRank(node.music(0) * (1+node.asInstanceOf[NodeImpl].splice.com) * nPosCompletelyTrue*nPosCompletelyTrue)
			}
			else
			{
				val falseness = node.falseLeads.size
				node.setRank(node.music(0) * (1+node.asInstanceOf[NodeImpl].splice.com) * 100.0 / falseness)
			}
		}
	}

	def rankNodes()
	{
		out.println("Ranking nodes...")
		populateCombinedLeadSets()
		val progress = new TableBuildProgress(500, out)
		for (compNode <- compPlan.calling)
		{
			val lh = compNode.startLH
			for (node <- nodeAlternatives(lh))
			{
				rankNode(node)
				progress.emit
			}
			nodeAlternatives+= lh -> nodeAlternatives(lh).sortBy(-_.rank)
		}
		out.println()
		altCombinedLeadSets.clear()
	}

	/**
	 * Marshalls a set of altNodes into a tree structure based on the order of their leads: e.g. for 8-spliced the
	 * root of the tree might be the 8 possible starting leads. This tree structure is designed to optimise the process
	 * of identifying whether nodes from a different position can be true against all alts in this position: we can quickly
	 * identify branches which are completely false.
	 */
	class LeadTree(remainingTree: List[List[Int]])
	{
		val count = remainingTree.size
		val leaves: Map[Int, LeadTree] = makeLeaves()

		private def makeLeaves() =
		{
			val headTree = remainingTree.filter{!_.isEmpty}.groupBy(_.head)
			headTree.mapValues((xs)=> new LeadTree(xs.map{_.tail})).toMap
		}

		def canBeTrue(node: Node): Boolean =
		{
			if (leaves.isEmpty)
				true
			else
				leaves.exists{(x)=> !node.falseLeads.contains(x._1) && x._2.canBeTrue(node)}
		}

		def nFalse(node: Node): Int =
		{
			def countFalse(lead: Int, tree: LeadTree) = if (node.falseLeads.contains(lead)) tree.count else tree.nFalse(node)
			leaves.map{(x)=> countFalse(x._1, x._2)}.sum
		}
	}

	/**
	 * Like LeadTree but actually keeps a list of the Nodes at the leaves, rather than just a count.
	 * Used for rapid location of a random true node in the search process.
	 * @param remainingTree
	 */
	class NodeTree(remainingTree: List[(Node, List[Int])])
	{
		val count = remainingTree.size
		val nodes: List[Node] = remainingTree.filter{_._2.isEmpty}.map{_._1}
		val leaves: Map[Int, NodeTree] = makeLeaves()

		private def makeLeaves() =
		{
			def stripLead(p: (Node, List[Int])) = (p._1, p._2.tail)
			val headTree = remainingTree.filter{!_._2.isEmpty}.groupBy(_._2.head)
			headTree.mapValues((xs)=> new NodeTree(xs.map{stripLead})).toMap
		}

		/** By number of nodes in each leaf */
		def findRandomTrue1(falseLeads: BitSet): Option[Node] =
		{
			var n = nodes.size;
			val trueLeaves = leaves.keys.filter{!falseLeads.contains(_)}.toList
			n+= trueLeaves.map{leaves(_).count}.sum
			if (n==0)
				None
			else
			{
				var r = Random.nextInt(n)
				if (r<nodes.size)
					Some(nodes(r))
				else
				{
					r-= nodes.size
					val it = trueLeaves.iterator
					var leaf = leaves(it.next())
					while (r>=leaf.count)
					{
						r-= leaf.count
						leaf = leaves(it.next())
					}
					leaf.findRandomTrue1(falseLeads)
				}
			}
		}
		/** By number of leaves */
		def findRandomTrue2(falseLeads: BitSet): Option[Node] =
		{
			val trueLeaves = leaves.keys.filter{!falseLeads.contains(_)}.toArray
			val immediate = if (nodes.size==0) 0 else 1
			val n = trueLeaves.size + immediate
			if (n==0)
				None
			else
			{
				val r = Random.nextInt(n)
				if (r==n-1 && immediate>0)
					Some(nodes(Random.nextInt(nodes.size)))
				else
					leaves(trueLeaves(r)).findRandomTrue2(falseLeads)
			}
		}

	}

	/** More efficient if smaller set first */
	private def trueSets[T](one: Set[T], two: Set[T]): Boolean = one.forall{!two(_)}

	private def trueSets(one: Set[Int], two: mutable.BitSet): Boolean = one.forall{!two(_)}

	def time(name: String, fnToTime: => Unit)
	{
		val t = System.currentTimeMillis()
		fnToTime
		out.println(name+" in "+(System.currentTimeMillis()-t)+"ms")
	}


}

case class Lead(startLH: Row, method: NamedMethod) extends Numberable
{
	val falseLeads = mutable.BitSet()
	var internallyFalse = false
	var containsRounds = false
	var music: MusicCount = _
	/** Can be more than one for multiparts - refers to individual leads, not all leads for the part */
	var nleadsWithoutMusic = 0

	def getRows =
	{
		val buf = ListBuffer[Row]()
		method.generateLead(startLH, buf)
		buf.toList
	}
}

class LeadTable extends Numberer[Lead]