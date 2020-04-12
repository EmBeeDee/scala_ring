package net.snowtiger.special.stedmancinques

import net.snowtiger.ringing.Row

import scala.collection.mutable

/**
 * @author mark
 */

object LBStedman
{
	val allCallings = mutable.Map[Row,List[String]]()
	/** Course end position expected for the 6th */
	val sixthHomePos = 5
	val maxCallsPerCourse = 3

	def main(args: Array[String]): Unit =
	{
		findAllCallings
		//allCallings = allCallings.sortBy(_.size).filter{_.size<15}
		findCompFrom(Row("234165"))
		//findCompFrom(Row("413256"))
	}

	def findCompFrom(start: Row): Unit =
	{
		val best = mutable.Map[Int,Float]()

		def output(revCourses: List[StedmanCourse]): Unit =
		{
			val totalMusic = revCourses.map{_.musicScore}.sum
			val length = revCourses.size
			val musicPerCourse = totalMusic.toFloat/length
			if (musicPerCourse>=best.getOrElse(length, 0.0f))
			{
				best+= length->musicPerCourse
				val total56 = revCourses.map{_.score56}.sum
				val totalBackLB5 = revCourses.map{_.scoreBackLB5}.sum
				println(revCourses.size+" courses "+totalMusic+"/"+totalBackLB5+"/"+total56+" "+musicPerCourse)
				for (course <- revCourses.reverse)
					println(course)
				println
			}
		}
		def recurse(ch: Row, truthSet: Set[String], revCourses: List[StedmanCourse]): Unit =
		{
			if (revCourses.size<13)
			{
				val musicalCourses = findMusicalCallings(ch, truthSet)
				for (course <- musicalCourses)
				{
					val courseHead = course.courseEnd
					if (courseHead==start && revCourses.size>=12)
						output(course :: revCourses)
					else
						recurse(courseHead, truthSet ++ course.truthRows, course :: revCourses)
				}
			}
		}

		recurse(start, Set(), Nil)
	}

	def findMusicalCallings(courseHead: Row, truthSet: Set[String]): List[StedmanCourse] =
	{
		def findBestCalling(callings: List[String]): Option[StedmanCourse] =
		{
			val musicalCourses = callings.flatMap{StedmanCourse.ifMusical(courseHead, _)}
			val trueCourses = musicalCourses.filter{_.trueWith(truthSet)}
			if (trueCourses.isEmpty)
				None
			else
				Some(trueCourses.minBy{_.sortPreference})
		}

		allCallings.values.flatMap(findBestCalling).toList.sortBy{_.sortPreference}
	}

	def findAllCallings =
	{
		def recurse(remaining: List[Int], revCalling: List[String]): Unit = remaining match
		{
			case head::tail => {
				recurse(tail, revCalling)
				if (head<9 || head>16)
					recurse(tail, (""+head)::revCalling)
				recurse(tail, ("s"+head)::revCalling)
			}
			case Nil => {
				val calling = revCalling.reverse.mkString(" ")
				val course = StedmanCourse("123456", calling)
				val courseEnd = course.courseEnd
				if (courseEnd.bellAt(sixthHomePos)==sixthHomePos && !courseEnd.isRounds && revCalling.size<=maxCallsPerCourse)
				{
					allCallings+= courseEnd->(calling::allCallings.getOrElse(courseEnd, Nil))
					//println(courseEnd+" "+calling)
				}
			}
		}

		println("Callings: ")
		recurse(StedmanCourse.positions, Nil)
		println("Total = "+allCallings.values.map{_.size}.sum+" unique courseheads = "+allCallings.size)
	}
}