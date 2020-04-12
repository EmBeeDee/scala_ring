package net.snowtiger.spliced.atw

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * @author mark
 */

abstract class AtwMethodFinderBase2[NodeType](nbells: Int)
{
	protected def makeNode(mc: MethodCourse, sm: SearchMethod): Option[NodeType]

	val sliceSize = 11
	def makeSearchMethods(methods: List[NamedMethod]) =
		methods.map{FullSearchMethod(_)}
	//methods.groupBy{_.lead.slice(0,sliceSize)}.values.toList.map{FirstSectionSearchMethod(sliceSize,_)}

	/** Builds method courses for a given (naturally-ordered) splice  */
	def buildMethodCourses(courseSet: Int, cos: List[CoursingOrder], splice: List[String]) =
	{
		/** This is in splice order */
		val leadNums: List[Int] = CourseStructure.lhGroupsToLeadNums(nbells, splice)
		val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		methodCourses
	}

	/** Builds the method nodes for a given (naturally-ordered) splice  */
	def buildNodes(courseSet: Int, cos: List[CoursingOrder], searchMethods: Map[String,List[SearchMethod]], splice: List[String]) =
	{
		/** This is in splice order */
		val leadNums: List[Int] = CourseStructure.lhGroupsToLeadNums(nbells, splice)
		val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		var revNodes = List[Set[NodeType]]()
		for ( (mc,lhg) <- methodCourses.zip(splice))
			revNodes = makeNodes(lhg, mc, searchMethods)::revNodes
		revNodes.reverse
	}

	/** Builds seed nodes for a given (naturally-ordered) splice  */
	def buildSeedNodes(courseSet: Int, cos: List[CoursingOrder], seedMethods: List[SearchMethod], splice: List[String]) =
	{
		/** This is in splice order */
		val leadNums: List[Int] = CourseStructure.lhGroupsToLeadNums(nbells, splice)
		val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		var revNodes = List[NodeType]()
		for ( (mc,sm) <- methodCourses.zip(seedMethods))
			revNodes = makeNode(mc, sm).get::revNodes
		revNodes.reverse
	}

	private def makeNodes(lhg: String, mc: MethodCourse, searchMethods: Map[String,List[SearchMethod]]): Set[NodeType] =
	{
		searchMethods(lhg).flatMap{makeNode(mc, _)}.toSet
	}

	def findMostPopularSplice(comp: InputComp, searchMethods: Array[Map[String,List[SearchMethod]]]) =
	{
		val trees = comp.courses.map{new LHGTreeBuilder(nbells, _)}
		def mostPopularInTree(treeBuilder: LHGTreeBuilder, courseSet: Int) =
		{
			/** This is in monotonically increasing order, NOT splice order */
			val sortedLeadNums: List[Int] = treeBuilder.leadNumSets.last
			val lhgTree = treeBuilder.lhgTrees.last
			// First get the most popular LH groups, in leadNum order
			var revLHGs = List[String]()
			var currTree = lhgTree
			while (!currTree.lhGroups.isEmpty)
			{
				val mostPopularLHG = currTree.lhGroups.keySet.maxBy{searchMethods(courseSet).getOrElse(_,Set()).size}
				revLHGs = mostPopularLHG::revLHGs
				currTree = currTree.lhGroups(mostPopularLHG)
			}
			val lhgsInLeadNumOrder = revLHGs.reverse
			val leadNumToLHGroup = sortedLeadNums.zip(lhgsInLeadNumOrder).toMap
			// Now re-order the LH groups into splice order
			var leadNum = 0
			var lhg = lhgsInLeadNumOrder.head
			revLHGs = Nil
			var finished = false
			while (!finished)
			{
				revLHGs = lhg::revLHGs
				leadNum = (leadNum + CourseStructure.LHOrders(lhg) + nbells-1) % (nbells-1)
				if (leadNum==0)
					finished = true
				else
					lhg = leadNumToLHGroup(leadNum)
			}
			// Now in splice order!
			revLHGs.reverse
		}
		trees.zipWithIndex.map{(p)=> mostPopularInTree(p._1, p._2)}
	}


}