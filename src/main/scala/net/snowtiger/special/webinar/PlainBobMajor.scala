package net.snowtiger.special.webinar

import net.snowtiger.ringing._

/**
 * @author mark
 */

object PlainBobMajor
{
	val method = NamedMethod("Plain", 8, "-18-18-18-18", "12")
	val plainPerm = method.plainPerm
	val bobPerm = method.callPerm(PN("14"))
	val twoPlains = plainPerm*2
	val threePlains = plainPerm*3
	val bobAndTwo = twoPlains.permuteBy(bobPerm)
	//val MaxLen = 2000
	var nFound = 0

	def main(args: Array[String]): Unit =
	{
		searchBefore(Row(8).apply(threePlains), "", Set())
		searchBefore(Row(8).apply(bobAndTwo), "", Set())
	}

	def searchWrong(prev: Row, comp: String, found: Set[Row]): Unit =
	{
		val newFound = found+prev
		if (newFound.size>found.size)
		{
			val compWithCourseEnd = comp+" "+prev.toString.substring(1,6)+" "
			if (prev.isRounds)
				printComp(compWithCourseEnd)
			else
			{
				searchBefore(prev.apply(threePlains), compWithCourseEnd, newFound)
				searchBefore(prev.apply(bobAndTwo), compWithCourseEnd+"W", newFound)
			}
		}
	}

	def searchBefore(prev: Row, comp: String, found: Set[Row]): Unit =
	{
		val newFound = found+prev
		if (newFound.size>found.size)
		{
			searchMiddle(prev.apply(twoPlains), comp, newFound)
			//searchBefore(prev.apply(bobPerm), comp+"B", newFound)
		}
	}

	def searchMiddle(prev: Row, comp: String, found: Set[Row]): Unit =
	{
		val newFound = found+prev
		if (newFound.size>found.size)
		{
			searchHome(prev.apply(plainPerm), comp, newFound)
			searchHome(prev.apply(bobPerm), comp+"M", newFound)
		}
	}

	def searchHome(prev: Row, comp: String, found: Set[Row]): Unit =
	{
		val newFound = found+prev
		if (newFound.size>found.size)
		{
			searchWrong(prev.apply(plainPerm), comp, newFound)
			searchWrong(prev.apply(bobPerm), comp+"H", newFound)
		}
	}

	def printComp(comp: String): Unit =
	{
		val callings = "WMH"
		val spacer = "\t"

		case class Course(calling: Array[Int], courseEnd: String)
		{
			val callPrint = Map(0->" ", 1->"-", 2->"2", 3->"3")

			def combine(nextCourse: Course): Option[Course] =
			{
				val last1 = calling.reverse.dropWhile(_==0).length - 1
				val first2 = nextCourse.calling.length - nextCourse.calling.dropWhile(_==0).length
				if (first2<last1)
					None
				else
					Some( Course(calling.zip(nextCourse.calling).map{(p)=> p._1+p._2}, nextCourse.courseEnd) )
			}

			override def toString =
			{
				courseEnd + calling.map((c)=> callPrint(c)).mkString(spacer, spacer, "");
			}
		}

		def makeCourse(callingAndCourseEnd: Array[String]): Course =
		{
			val calls = new Array[Int](callings.length)
			var calling = callingAndCourseEnd(0)
			var i = 0
			for (c <- callings)
			{
				if (calling.startsWith("" + c))
				{
					calls(i) = 1
					calling = calling.tail
				}
				else
					calls(i) = 0
				i+= 1
			}
			Course(calls, callingAndCourseEnd(1))
		}

		def combineCourses(courses: List[Course]): List[Course] =
		{
			courses.tail.foldLeft(List(courses.head)){(reversed: List[Course], next: Course)=>
				reversed.head.combine(next) match
				{
					case None => next::reversed
					case Some(comb) => comb::reversed.tail
				}
			}.reverse
		}

		val courses = combineCourses(comp.split(' ').grouped(2).map(makeCourse).toList)
		nFound+= 1
		println(""+(courses.size*112)+" Plain Bob Major no. "+nFound)
		println("23456"+spacer+"W"+spacer+"M"+spacer+"H")
		for (course <- courses)
			println(course)
		println()
		Thread.sleep(1);
	}
}