package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.atw.AtwMethodFinder.MethodProvider
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.collection.mutable.ListBuffer

/**
 * This one uses Knuth's Dancing Links exact-cover algorithm. The lead heads and ends form the mandatory
 * columns, with all the remaining rows of the extent being optional columns. The rows of the matrix are
 * individual method choices. Although the mandatory columns ensure that 23 methods are rung, one for
 * each course, it does not stop the same method being chosen more than once.
 *
 * In practice the matrix size is far too big for this algorithm, operating on the full search space.
 *
 *
 * @author mark
 */
class AtwMethodFinder6(nbells: Int) extends AtwMethodFinderBase2[MethodNode](nbells)
{
	override protected def makeNode(mc: MethodCourse, sm: SearchMethod) =
	{
		val mn = new MethodNode(mc, sm)
		if (mn.isTrue) Some(mn) else None
	}

	def findMethods(methodProvider: MethodProvider, comp: InputComp)
	{
		val searchMethods: Array[Map[String,List[SearchMethod]]] =
			(0 until comp.courses.size).toArray.map{(n)=> methodProvider.methodsForCourseSet(n).mapValues(makeSearchMethods).view.force}

		println("Building method nodes...")
		val lhGroups = findMostPopularSplice(comp, searchMethods)
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		var methodNodes = lhGroups.zipWithIndex.map{(p)=> buildNodes(p._2, cos(p._2), searchMethods(p._2), p._1)}.toList.flatten

		// May be no methods for a given node if rowsAlreadyFound have been set, and are false with all possibilities.
		if (methodNodes.exists(_.isEmpty))
			println("No methods exist for at least one node - aborting search")
		else
		{
			println("Building matrix...")
			methodNodes = methodNodes.slice(0,21)
			val mandatoryRows = methodNodes.map{(set)=> getMandatoryRows(methodProvider, set.head)}
			val dancingLinks = new DancingLinks
			dancingLinks.createMatrix(mandatoryRows.flatten)
			for (nodeList <- methodNodes; node <- nodeList)
				dancingLinks.addMethodRow(node)

			println("Searching...")
			var t = System.currentTimeMillis()
			dancingLinks.search

			t = (System.currentTimeMillis()-t)/1000
			println("Search finished in "+t+"s")
		}
	}

	def getMandatoryRows(methodProvider: MethodProvider, node: MethodNode) =
	{
		val nm = methodProvider.allMethods(node.method.lhGroup).head
		val sm = FirstSectionSearchMethod(0, List(nm))
		val buf = ListBuffer[Row]()
		for (lh <- node.course.leadheads)
			sm.generateLead(lh, buf)
		buf.toList
	}

}