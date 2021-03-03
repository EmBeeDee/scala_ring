package net.snowtiger.spliced.atw

/**
 * @author mark
 */

/** A tree, whose depth equals the length of the splice (vertical) course, and whose branches are in the same order
	* as the sorted leadNums; each branch gives the available LH groups for that leadNum, given previous choices. */
case class LHGTree(lhGroups: Map[String,LHGTree])

/** Given multiple splices for a given course set, finds the common set of lead numbers, and builds a tree
	* of available LH groups from each lead number. For example, splices CABFB and DAECA both visit the same
	* lead numbers, 0 3 4 5 6, but in a different order. If we search for compositions using the lead numbers
	* 0 3 4 5 6, then either of the LH groups C or D is available for lead 0. However, if we choose C, then
	* the next lead 3 must be A, and the remaining choices are fixed; conversely, choosing D gives a different
	* set of possible LH groups, starting from 3=C.
	* <p>
	* Finally, once we have found a composition (set of methods that have LH groups matching a splice), how do
	* we get back to the original splice and hence the order of methods? The order of output will be the sorted
	* order of the leadnums, so e.g. if the final splice consists entirely of f-type methods, these will be listed
	* in reverse order (leadnums 0 1 2 3 4 5 6 given). When writing out the composition, re-order the methods
	* by following the actual lh groups from leadnum 0.
	*
	*/
class LHGTreeBuilder(nbells: Int, course: InputCourse)
{
	/** The length of the shortest splice for the course set */
	val spliceLength = course.spliceList.minBy(_.length).length
	/** Retain only the splices of minimum length */
	val shortestSplices = course.spliceList.filter{_.length==spliceLength}

	/** Turn each remaining splice into a CourseStructure, which tells us which lead nums it visits and with which LH groups */
	val courseStructures = shortestSplices.map{CourseStructure(nbells, _)}

	/** Now find the unique sets of lead numbers which the splices traverse. Many different splices will hit the same lead numbers. */
	val courseStructuresByLeadsVisited = courseStructures.groupBy(_.leadsVisited)
	val leadNumSets: List[List[Int]] = courseStructuresByLeadsVisited.keys.toList
	println(leadNumSets.mkString(" "))

	/** For each set of lead numbers, we'll build a tree. Start by grouping all the course structures for each leadNum-visited set. */
	val lhGroupSets: List[List[CourseStructure]] = leadNumSets.map{courseStructuresByLeadsVisited(_)}

	private def buildTree(leadNums: List[Int], structures: List[CourseStructure]): LHGTree =
	{
		val branches = leadNums match
		{
			case Nil => Map[String,LHGTree]()
			case leadNum::tail => structures.groupBy(_.leadMap(leadNum)).mapValues{(xs)=> buildTree(leadNums.tail, xs)}
		}
		LHGTree(branches.toMap)
	}

	/** Finally, the leadhead group tree itself: basically a Map from (LH groups available at a given level) -> (tree at the next level).
		* There may be more than one tree if the course structures containing more than one set of distinct leads visited */
	val lhgTrees: List[LHGTree] = leadNumSets.zip(lhGroupSets).map{(p)=> buildTree(p._1, p._2)}

	/** Also list all the LH groups which are possible for each leadNum - per splice tree, if multiple. */
	private def allLHsAtLeadNum(leadNum: Int, structures: List[CourseStructure]): Set[String] = structures.map{_.leadMap(leadNum)}.toSet
	val allLHGroups: List[List[Set[String]]] = leadNumSets.zip(lhGroupSets).map{(p)=> p._1.map(allLHsAtLeadNum(_, p._2))}

}
