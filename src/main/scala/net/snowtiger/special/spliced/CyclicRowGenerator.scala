package net.snowtiger.special.spliced

import net.snowtiger.ringing.{NamedMethod, Row}

import scala.collection.mutable.ListBuffer

/**
 * @author mark
 */

class CyclicRowGenerator(method: NamedMethod)
{
	val cyclicCourses = List(("1234567890ET", 1),
		("134567890ET2", 1), ("14567890ET23", 1), ("1567890ET234", 1), ("167890ET2345", 1), ("17890ET23456", 1),
		("1890ET234567", 1), ("190ET2345678", 1), ("10ET23456789", 1), ("1ET234567890", 1), ("1T234567890E", 1))

	def genRows(courses: List[(String,Int)]): Set[Row] = genRows(courses, method)

	def genRows(courses: List[(String,Int)], meth: NamedMethod): Set[Row] =
	{
		val rows = new ListBuffer[Row]()
		for (course <- courses)
		{
			var lh = Row(course._1)
			var n = course._2
			while (n>0)
			{
				lh = meth.generateLead(lh, rows)
				n-= 1
			}
		}
		val rowSet = rows.toSet
		assert(rowSet.size==rows.size)
		rowSet
	}
}

object CyclicRowGenerator
{
	def genCyclicCourseEnds(nbells: Int) =
	{
		val chs = new ListBuffer[Row]()
		val rounds = new Row(nbells)
		var ch = rounds
		while (chs.isEmpty || ch!=rounds)
		{
			chs+= ch
			ch = Row("1" + ch.extract(3, nbells).toString + ch.extract(2, 2).toString)
		}
		chs.toList
	}
}