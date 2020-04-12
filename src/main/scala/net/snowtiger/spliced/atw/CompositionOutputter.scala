package net.snowtiger.spliced.atw

import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * @author mark
 */

class CompositionOutputter(comp: InputComp, lhGroups: List[List[String]])
{
	val nbells = comp.nbells
	val callingPositions = Map('H'->0, 'W'->1, 'M'->(nbells-2))
	val methods = comp.unflattenByCourse("ABCDEFGHIJKLMNOPQRSTUVW".toList)
	val courseSets = comp.courses.zip(lhGroups).zip(methods).map{(p)=> CourseSet(p._1._1, p._1._2, p._2)}
	val coursesByCO = courseSets.flatMap{_.courses}.map{(c)=> (CoursingOrder(c.co), c)}.toMap
	val calling = comp.calling.split(' ')

	def output =
	{
		var out = ""
		var prevCall = "H"
		var co = CoursingOrder("53246")
		for (call <- calling)
		{
			val course = coursesByCO(co)
			out+= course.methodsBetweenCalls(prevCall, call)
			if (call.startsWith("s"))
				out+="' "
			else
				out+="- "
			co = co.call(call)
			prevCall = call
		}
		out
	}

	case class CourseSet(inputCourse: InputCourse, splice: List[String], methods: List[Char])
	{
		val baseLeadNums = CourseStructure.lhGroupsToLeadNums(nbells, splice)
		val courses = inputCourse.coList.zipWithIndex.map{(p)=> buildCourse(p._1, p._2)}.toArray

		def shiftLeadnums(rot: Int) = baseLeadNums.map{(i)=> (i+rot)%(nbells-1)}
		def buildCourse(co: String, rot: Int) =
		{
			val leadNums = shiftLeadnums(rot)
			Course(co, leadNums, methods.mkString)
		}
	}

	case class Course(co: String, leadNums: List[Int], methods: String)
	{
		def methodsBetweenCalls(fromCall: String, toCall: String) =
		{
			val from = leadNums.indexOf(callingPositions(fromCall.last))
			val to = leadNums.indexOf(callingPositions(toCall.last))
			if (from<0 || to<0)
				println("Oops")
			if (from<to)
				methods.substring(from, to)
			else
			  methods.substring(from, methods.size) + methods.substring(0, to)
		}
	}
}