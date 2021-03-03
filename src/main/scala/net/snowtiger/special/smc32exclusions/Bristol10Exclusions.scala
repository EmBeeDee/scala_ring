package net.snowtiger.special.smc32exclusions

import net.snowtiger.ringing.{CompositeMusic, NamedMethod, Row}
import net.snowtiger.spliced.score.{Music65Rollup, MusicLB, MusicRun}

/**
 * @author mark
 */

object Bristol10Exclusions
{
	val bristol = NamedMethod("Bristol", 10, "-50-14.50-50.36.14-70.58.16-16.70-16-10", "10")
	// Positive and negative tenors-parts course ends
	val allCourseHeads = Row.generateAll(5).map((r) => r.shift(1).extendTo(10))
	val musicRun = new CompositeMusic(new MusicRun(5), new MusicLB(4), new Music65Rollup)

	val allCourses = allCourseHeads.map(bristol.generateFullCourse)
	val badCourses = allCourses.filter(!isGood(_))
	val badLeads = badCourses.mapConserve(_(0))

	def main(args: Array[String]): Unit =
	{
		println("nBad = " + badLeads.size)
		println("-1 0 " + badLeads.map(_.toString + "l").mkString(" "))
	}

	def isGood(rows: List[Row]): Boolean =
		musicRun.countMusic(rows) >= 2
}
