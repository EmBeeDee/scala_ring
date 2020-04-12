package net.snowtiger.special.smc32exclusions

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.NiceMethods
import net.snowtiger.spliced.score.MusicRun

/**
 * @author mark
 */

object Bristol8Exclusions extends NiceMethods
{
	// Positive tenors-parts course ends
	val allCourseHeads = Row.generateAll(6).filter(_.positive).map((r)=> r.shift(1).extendTo(8))
	val musicRun = new MusicRun(4)

	//val niceBackBellCOs = Set("6875", "8756", "5687", "5876", "8765", "6587", "8657", "7568", "8576", "6758", "5678", "8765", "6857", "7586")
	val niceBackBellCOs = Set("6875", "8756", "5687", "5876", "8765", "6587", "8657", "7568", "8576", "5678")

	val allCourses = allCourseHeads.map(bristol.generateFullCourse)
	val badFirstHalves = allCourses.map(_.slice(32, 96)).filter(!isGood(_))
	val badSecondHalves = allCourses.map(_.slice(128, 196)).filter(!isGood(_))

	val badFirstHalfLeads = badFirstHalves.map(_(32))
	val badSecondHalfLeads = badSecondHalves.map(_(0))

	def main(args: Array[String]): Unit =
	{
		println("nFirst = "+badFirstHalfLeads.size)
		println("nSecnd = "+badSecondHalfLeads.size)
		println("Bad first half course")
		println("-1 0 "+badFirstHalfLeads.map(_.toString+"l").mkString(" "))
		println("Bad second half course")
		println("-1 0 "+badSecondHalfLeads.map(_.toString+"l").mkString(" "))

	}

	def isGood(rows: List[Row]): Boolean =
	{
		niceBackBellCO(rows.head.coursingOrder(bristol.plainPerm)) || musicRun.countMusic(rows)>=2
	}

	def niceBackBellCO(co: String): Boolean =
	{
		val lbs = Array(co.indexOf("2"), co.indexOf("3"), co.indexOf("4")).sorted
		val dist = lbs(2)-lbs(0)
		dist==2 && (co.length==5 || niceBackBellCOs(co.substring(lbs(2)+1)+co.substring(0, lbs(0))))
	}

}