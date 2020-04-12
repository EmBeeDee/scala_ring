package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{Perm, Row}
import net.snowtiger.spliced.composition.Stage
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * @author mark
 */
object NewAtwGroupFinder extends AtwHelper
{
	val nbells = 8

	val genMixedSignCourses = false

	val courseheads = (if (genMixedSignCourses) genAllCourseHeads(nbells) else genPositiveCourseHeads(nbells)).toSet

	val plainCourseLeads = new Stage(nbells).StageLeadheads
	val firstLeadPerm = plainCourseLeads.tail.head.toPerm
	val plainCourseCO = CoursingOrder("53246")

	val allCOs = courseheads.map{(lh) => CoursingOrder(lh.coursingOrder(firstLeadPerm))}.toList.sortBy(_.raw)

	def makeCourse(co: CoursingOrder): Array[Row] =
	{
		var revCourse = List(co.toCourseHead(nbells))
		for (i <- 1 until nbells)
			revCourse = revCourse.head.apply(firstLeadPerm)::revCourse
		revCourse.reverse.toArray
	}
	val allCourses = Map[CoursingOrder, Array[Row]]() ++ allCOs.zip(allCOs.map(makeCourse))

	val protoMethod = ProtoMethod('A', firstLeadPerm)

	var hiwater = 0

	def main(args: Array[String]): Unit =
	{
		val atw = new SingleMethodAtw(protoMethod) + Row(nbells)
		findAtwCOs(atw, List(plainCourseCO), 1, 2*(nbells-1))
	}

	def findAtwCOs(atw: SingleMethodAtw, revCosFound: List[CoursingOrder], leadNum: Int, requiredAtw: Int): Unit =
	{
		if (leadNum==7)
			output(revCosFound)
		else
			for (co <- allCourses.keys)
			{
				val newLead = allCourses(co)(leadNum)
				val newAtw = atw + newLead
				if (newAtw.score==requiredAtw)
					findAtwCOs(newAtw, co::revCosFound, leadNum+1, requiredAtw+7)
			}
	}

	def output(revCosFound: List[CoursingOrder]): Unit =
	{
		val cosFound = revCosFound.reverse
		val perms = cosFound.map{_.toCourseHead(nbells).toPerm}
		val order = groupSize(perms)
		val coSet = cosFound.toSet
		val fixed = "23456".filter{(c)=> cosFound.forall(_.raw.indexOf(c)=="53246".indexOf(c))}
		val counts = coSet.map{(co)=> (co, cosFound.count(_==co))}.filter{_._2 > 1}
		if (order<24)
			println("Solution: "+revCosFound.reverse.mkString(", ")+" n = "+coSet.size+" order = "+order+" fixed = "+fixed+" "+counts.mkString(" "))
	}

	def groupSize(perms: List[Perm]) =
	{
		var group = Set(Row(nbells).toPerm)
		var remainingPerms = perms.toSet
		while (remainingPerms.nonEmpty)
		{
			val perm = remainingPerms.head
			remainingPerms-= perm
			val newPerms = group.map(_.permuteBy(perm)).diff(group)
			group++= newPerms
			remainingPerms++= newPerms
		}
		group.size
	}
}