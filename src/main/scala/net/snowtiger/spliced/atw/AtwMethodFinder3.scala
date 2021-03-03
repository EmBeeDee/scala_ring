package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.atw.AtwMethodFinder.MethodProvider
import net.snowtiger.spliced.atw.construct.{CompositionCompleter, LastSectionBuilder, MethodLeads}
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.collection.BitSet


/**
 * A method finder for ATW compositions which searches all available splices at once, by determining the common
 * set of lead numbers which they visit (usually there is only one such set per ATW course-set). It builds a tree
 * of the available LH groups for each successive method choice in the composition, based on the splices, so that
 * even whilst searching through the lead numbers (and hence leadheads) alone, we can be sure that a consistent
 * set of LH groups will be found for each course-set, matching one of the original splices.
 * <p>
 * The search is also carried out using the initial section of the methods only, in order to reduce the number of
 * methods considered. The {@code NFirstSections} constant gives the length of the initial prefix of the method to take -
 * e.g. setting to 1 will take the first section, hence the first five rows of the lead; since we are searching for
 * specific LH groups, we also calculate the last five rows of the lead too, and add these into the falseness calculations.
 * This does not increase the number of method nodes, nor does it greatly increase the truthtable build time, but it
 * does allow much faster pruning especially for negative courses which are false between sections in opposite halves
 * of the lead.
 * <p>
 * If a given list of method sections is found which is true to the entire composition, this "probe" composition
 * is then expanded to full methods, if possible. The probe search followed by the probe expansion are interleaved
 * recursively in order to try and optimise the pruning characteristics of both searches.
 * <p>
 * Recently altered to allow the {@link CompositionCompleter} to go to work once the probe has finished.
 *
 * @author mark
 */
class AtwMethodFinder3(nbells: Int) extends AtwMethodFinderBase
{
	val NFirstSections = 3
	val MaxDupPrefixes = 1
	val PrefixLength = SectionLengths(NFirstSections)
	val ExpandProbes = false
	val CompleteProbes = true
	val MinNextLevelNodes = 1

	println("Number of prefix sections = "+NFirstSections+", prefix length = "+PrefixLength)

	var nMethodsRequired = 0
	var bestScore = 0
	var highwater = 0
	var highwater2 = 0

	var popularMethods = Map[SearchMethod, Int]()

	var inputComp: InputComp = _
	var leadNumSets: List[List[Int]] = _

	// section size -> methods per node
	// 7 -> 8 but no solutions for comp 1 - hiwater 13
	// 8 -> 6
	// 10 -> 4 NONE
	def goodChoiceNode(node: NumberedMethodNode) = node.nextLevelNodes.size>=MinNextLevelNodes

	def findMethods(methodProvider: MethodProvider, comp: InputComp)
	{
		highwater = 0
		highwater2 = 0
		bestScore = 0
		popularMethods = popularMethods.empty
		inputComp = comp

		println("Building splice trees")
		val trees = comp.courses.map{new LHGTreeBuilder(nbells, _)}
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		//val searchMethods: Map[String,List[SearchMethod]] = methodProvider.allMethods.mapValues(makeSearchMethods)
		val searchMethods: Array[Map[String,List[SearchMethod]]] = (0 until comp.courses.size).toArray.map{(n)=> methodProvider.methodsForCourseSet(n).mapValues(makeSearchMethods).toMap}
		println("Building method-section nodes...")
		val nodeSetBuilders = trees.zipWithIndex.map{(p)=> new NodeSetBuilder(p._2, cos(p._2), searchMethods(p._2), p._1)}
		val methodNodes = nodeSetBuilders.flatMap{_.methodNodes}.map{_.filter(goodChoiceNode)}
		nMethodsRequired = methodNodes.size
		// May be no methods for a given node if rowsAlreadyFound have been set, and are false with all possibilities.
		if (methodNodes.exists(_.isEmpty))
			println("No methods exist for at least one node - aborting search")
		else
		{
			// Now build truth tables and prune false
			val prunedNodes = buildTruthTables(methodNodes)
			// Get rid of methods with too few true links
			//prunedNodes = removePoorlyLinkedNodes(prunedNodes, AtwMethodFinder.KeepBestAfter)
			// Finally build the node tree - will link to the node trees for the other course sets, so only the first required.
			val nodeTree = buildNodeTrees(prunedNodes, nodeSetBuilders.map{_.lhgTree}, comp)
			// Set up available nodes list for the first search depth
			val available = nodeSetBuilders.flatMap{_.methodCourses}.zip(prunedNodes).map{(p)=> AvailableNodes(p._1, BitSet()++p._2.map(_.n))}
			nodesAvailable = new Array[List[AvailableNodes]](nMethodsRequired+1)
			nodesAvailable(0) = available
			// And search
			println("Searching...")
			var t = System.currentTimeMillis()
			leadNumSets = nodeSetBuilders.map{_.leadNums}
			println(leadNumSets.flatten.mkString(" "))
			probeSearch(FullComposition.empty, ProbeComposition.empty, nodeTree)
			t = (System.currentTimeMillis()-t)/1000
			println("Search finished in "+t+"s")
		}
		println("Most popular methods ("+popularMethods.size+"):")
		for ( (m,c) <- popularMethods.toList.sortBy(-_._2))
			println(c+" "+m)
	}

	// Keep separate list of available nodes for each search depth, so we can return to old values when expanding probes
	var nodesAvailable: Array[List[AvailableNodes]] = _

	def makeSearchMethods(methods: List[NamedMethod]) =
		methods.groupBy{_.lead.slice(0,PrefixLength)}.values.toList.map{FirstSectionSearchMethod(PrefixLength,_)}

	var probeCount: Long = 0
	var fullCount: Long = 0

	def probeSearch(fullComp: FullComposition, probedComp: ProbeComposition, toProbe: NodeTree): Boolean =
	{
		def notDup(node: NumberedMethodNode) =
			fullComp.countSamePrefixes(node,PrefixLength)+probedComp.countSamePrefixes(node,PrefixLength)<=MaxDupPrefixes-1

		val i = fullComp.revFound.size + probedComp.probed.size
		if (i>highwater2)
		{
			highwater2 = i
			if (i==1)
				print("Probe highwater:")
			print(" "+highwater2)
			if (i==nMethodsRequired)
				println(" "+fullComp.revFound.reverse.mkString(", ")+" / "+probedComp.probed.reverse.mkString(", "))
		}
		if (i==nMethodsRequired)
		{
			probeCount += 1
			if (probeCount % 100000 == 0)
				println("Count: " + fullCount + " " + fullComp.revFound.reverse.mkString(", ") + " / " + probedComp.probed.reverse.mkString(", "))
			if (ExpandProbes)
			{
				expandProbe(fullComp, probedComp.reverse)
				true
			}
			else if (CompleteProbes)
			{
				val probed = probedComp.probed.reverse
				val lhGroups: List[List[String]] = inputComp.unflattenByCourse(probed.map{_.method.lhGroup})
				val courseStructures = leadNumSets.zip(lhGroups).map{(p)=> CourseStructure(p._1, p._2)}
				val splices = courseStructures.map{_.splice(nbells)}
				val outputter = new CompositionOutputter(inputComp, splices)
				println(outputter.output)

				println("Completing: "+probed.mkString(", "))
				//completeProbe(probed)
				completeLastSection(probed)
				true
			}
			else
			{
				println(probedComp.probed.reverse.mkString(", "))
				false
			}
		}
		else if (toProbe.isEmpty)
		{
			false
		}
		else
		{
			var treesLeft = toProbe.branches
			var probeSucceeded = false
			while (!treesLeft.isEmpty && !probeSucceeded)
			{
				var (nextTree, nodes) = treesLeft.head
				nodes = nodes.sortBy{-_.nextLevelNodes.size}
				treesLeft = treesLeft.tail
				while (!nodes.isEmpty && !probeSucceeded)
				{
					val node = nodes.head
					nodes = nodes.tail
					val avail = nodesAvailable(i)
					if (avail.head.available.contains(node.n) && notDup(node))
					{
						val nextAvailable = avail.tail.map{_.withNode(node)}
						nodesAvailable(i+1) = nextAvailable
						if (nextAvailable.forall{!_.isEmpty})
						{
							val remainingToExplore = if (nodes.isEmpty) NodeTree(treesLeft) else NodeTree((nextTree,nodes)::treesLeft)
							val increasedProbe = probedComp.increase(node, remainingToExplore)
							probeSucceeded = probeSearch(fullComp, increasedProbe, nextTree)
						}
					}
				}
			}
			probeSucceeded
		}
	}

	def expandProbe(fullComp: FullComposition, probeComp: ProbeComposition)
	{
		def notFullDup(node: ChildMethodNode) = !fullComp.revFound.exists{_.method==node.method}
		//def notFullDup(node: ChildMethodNode) = notDup(node.parent, fullComp, probeComp)

		val i = fullComp.revFound.size
		if (i>highwater)
		{
			highwater = i
			if (i==1)
				print("Expand highwater:")
			print(" "+highwater)
			if (i==nMethodsRequired)
				println()
		}
		if (probeComp.probed.isEmpty)
			found(fullComp.revFound)
		else
		{
			val reducedProbe = probeComp.reduce
			for (fullNode <- probeComp.probed.head.nextLevelNodes; if notFullDup(fullNode))
			{
				val nextRows = fullComp.rows++fullNode.rows
				if (nextRows.size==fullComp.rows.size+fullNode.rows.size)
					expandProbe(FullComposition(fullNode::fullComp.revFound, nextRows), reducedProbe)
			}
			probeSearch(fullComp, ProbeComposition.empty, probeComp.leftToExplore.head)
		}
	}

	def completeProbe(probeComp: List[NumberedMethodNode])
	{
		//assert(nSections*4-1==FirstSectionSize)
		val partialComp = probeComp.map{(n)=> PartialMethodNodeBitSet(MethodNodeBitSet(n.course, n.method), NFirstSections)}
		val possibleHalfLeads = probeComp.map{(n)=> MethodLeads(n.course, n.method.lhGroup).halfleads}
		val completer = new CompositionCompleter(possibleHalfLeads)
		completer.complete(new CourseAtwComposition(){
			override def courses = partialComp
			// !!! I think with the idea that the initial composition is complete down to the end of a section, so no falseness with the bit to be completed.
			override def rows = MultiBitSet()
			override def energy = ???
			override def randomMove(nodeAlts: Array[Array[MethodNodeBitSet]]) = ???
		})
	}

	def completeLastSection(probeComp: List[NumberedMethodNode]): Unit =
	{
		val partialComp = probeComp.map{(n)=> PartialMethodNodeBitSet(MethodNodeBitSet(n.course, n.method), NFirstSections)}
		val possibleHalfLeads = probeComp.map{(n)=> MethodLeads(n.course, n.method.lhGroup).halfleads}
		val methodCourses = probeComp.map{_.course}
		val lastSectionBuilder = new LastSectionBuilder(methodCourses, possibleHalfLeads)
		lastSectionBuilder.completeComp(partialComp)
	}

	def found(revFound: List[TruthTableMethodNode])
	{
		fullCount+= 1
		for (m <- revFound)
			popularMethods+= m.method->(1+popularMethods.getOrElse(m.method, 0))

		val score = revFound.map{_.score}.sum
		if (score>=bestScore)
		{
			bestScore = score
			val comp = revFound.reverse
			println(score+" "+comp.mkString(", "))
		}
	}

	case class FullComposition(revFound: List[ChildMethodNode], rows: Set[Row])
	{
		def containsProbeNode(node: NumberedMethodNode) = revFound.exists{_.parent.method==node.method}
		def countSamePrefixes(node: NumberedMethodNode, prefix: Int) =
			revFound.count(node.method.pn.slice(0,prefix)==_.parent.method.pn.slice(0,prefix))
		override def toString = "FullComposition("+revFound.reverse.mkString(",")+")"
	}

	object FullComposition
	{
		def empty = FullComposition(Nil, Set())
	}

	case class ProbeComposition(probed: List[NumberedMethodNode], leftToExplore: List[NodeTree])
	{
		def reduce = ProbeComposition(probed.tail, leftToExplore.tail)
		def reverse = ProbeComposition(probed.reverse, leftToExplore.reverse)
		def increase(newNode: NumberedMethodNode, newTree: NodeTree) =
			ProbeComposition(newNode::probed, newTree::leftToExplore)
		def containsNode(node: NumberedMethodNode) = probed.exists{_.method==node.method}
		def countSamePrefixes(node: NumberedMethodNode, prefix: Int) =
			probed.count(node.method.pn.slice(0,prefix)==_.method.pn.slice(0,prefix))
	}

	object ProbeComposition
	{
		def empty = ProbeComposition(Nil, Nil)
	}

	def buildNodeTrees(methodNodes: List[Set[NumberedMethodNode]], lhgTrees: List[LHGTree], comp: InputComp) =
	{
		// Group the nodes back into (lhGroup -> method node lists) at the inner level
		val nodesByLHG = methodNodes.map{_.toList.groupBy(_.method.lhGroup)}
		// And at the outer level, accumulate back up into course sets
		val revCourseSetNodes = comp.unflattenByCourse(nodesByLHG).reverse
		// Build the node trees in reverse order, passing the previously-built node tree into the builder for the next,
		// so that its final nodes can be linked to the next node. We end up with the first tree, linked to all the others.
		var nodeTree = NodeTree.empty
		for ( (methodNodes, lhgTree) <- revCourseSetNodes.zip(lhgTrees.reverse))
			nodeTree = buildNodeTree(methodNodes, lhgTree, nodeTree)
		nodeTree
	}

	/** Build a NodeTree for one course set, linking its final nodes to the node tree for the next course set
		* (which is the empty tree for the last course set).
		*/
	def buildNodeTree(methodNodes: List[Map[String,List[NumberedMethodNode]]], lhgTree: LHGTree, nodeTreeNextCourseSet: NodeTree): NodeTree =
	{
		def buildTreeNodesPair(nodesLeft: List[Map[String,List[NumberedMethodNode]]], lhgPair: (String, LHGTree)) =
		{
			val nextTree = buildNodeTree(nodesLeft, lhgPair._2, nodeTreeNextCourseSet)
			val nextNodes = methodNodes.head.getOrElse(lhgPair._1, Nil)
			(nextTree, nextNodes)
		}
		methodNodes match
		{
			case Nil => nodeTreeNextCourseSet
			case nodes::tail => NodeTree(lhgTree.lhGroups.toList.map{(p)=> buildTreeNodesPair(tail, p)})
		}
	}

	/** Used to build the MethodNodes for each course, preparatory to truthTable building and pruning */
	class NodeSetBuilder(courseSet: Int, cos: List[CoursingOrder], searchMethods: Map[String,List[SearchMethod]], treeBuilder: LHGTreeBuilder)
	{
		// Currently just using the first leadNumSet for each course.
		// If there are more than one, could iterate over all with multiple CourseSetTreeSearches per course set
		val leadNums: List[Int] = treeBuilder.leadNumSets.head
		val lhGroups: List[Set[String]] = treeBuilder.allLHGroups.head
		val lhgTree = treeBuilder.lhgTrees.head
		val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		// Get rid of search methods whose single-method falseness in this course set is poor
		/*
		val singleMethodTruth = searchMethods.values.toList.flatten.map{(m)=> (m,singleMethodFalseness(m, methodCourses))}
		val sortedSearchMethods = singleMethodTruth.sortBy(_._2)
		val bestSearchMethods = sortedSearchMethods.drop((sortedSearchMethods.size*AtwMethodFinder.PruneWorstBefore).toInt).map(_._1).groupBy{_.lhGroup}
		*/
		val bestSearchMethods = searchMethods
		// For each leadNum, a list of all possible method nodes (only those with LH groups possible for the leadNum)
		private def makeNodes(lhgs: Set[String], mc: MethodCourse): Set[NumberedMethodNode] =
			lhgs.flatMap{(lhg)=> makeNumberedMethodNodes(mc, bestSearchMethods.getOrElse(lhg,Nil))}
		val methodNodes: List[Set[NumberedMethodNode]] = lhGroups.zip(methodCourses).map{(p)=> makeNodes(p._1, p._2)}
	}

	/** As the LHGTree, but at each branch level we also hold the list of MethodNodes available for each LH group */
	case class NodeTree(branches: List[(NodeTree,List[NumberedMethodNode])])
	{
		def isEmpty = branches.isEmpty

		override def toString = "NodeTree(branches="+branches.size+")"
	}

	object NodeTree
	{
		def empty = NodeTree(Nil)
	}

	case class AvailableNodes(methodCourse: MethodCourse, available: BitSet)
	{
		def isEmpty = available.isEmpty
		def withNode(node: NumberedMethodNode) = AvailableNodes(methodCourse, node.bitTruth(methodCourse).intersect(available))

		override def toString = "AvailableNodes("+methodCourse+","+available.size+")"
	}

}