package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.score.StandardMajorMusic

import scala.collection.SortedMap

/**
 * @author mark
 */

object SeedGenerator extends StandardMethods
{
	val nbells = 8

	val chs = courseHeads(nbells)
	val music = new StandardMajorMusic()

	def main(args: Array[String])
	{
		val bestFromEach = 28
		var best = Set[Row]()
		best++= bestCoursesForMethod(bristol, bestFromEach)
		best++= bestCoursesForMethod(cambridge, bestFromEach)
		best++= bestCoursesForMethod(lessness, bestFromEach)
		best++= bestCoursesForMethod(lincolnshire, bestFromEach)
		best++= bestCoursesForMethod(london, bestFromEach)
		best++= bestCoursesForMethod(pudsey, bestFromEach)
		best++= bestCoursesForMethod(superlative, bestFromEach)
		best++= bestCoursesForMethod(yorkshire, bestFromEach)
		println(best.size)
		println("Good")
		println("1 0 "+best.mkString("c, ")+"c")
		val worst = chs.toSet--best
		println("Bad")
		println("-100 0 "+worst.mkString("c, ")+"c")
	}

	def courseHeads(nbells: Int) = Row.genTenorsTogetherCourseHeads(nbells)

	def bestCoursesForMethod(method: NamedMethod, nCourses: Int) =
	{
		var best = SortedMap[Int,List[Row]]()
		for (ch <- chs)
		{
			val score = method.generateFullCourse(ch).map{music.countMusic(_)}.sum
			best+= score->(ch::best.getOrElse(score, List()))
		}
		//best.toList.reverse
		best.values.toList.reverse.flatten.take(nCourses)
	}
}