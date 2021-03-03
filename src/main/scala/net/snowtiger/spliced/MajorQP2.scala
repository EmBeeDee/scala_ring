package net.snowtiger.spliced

import net.snowtiger.spliced.composition.{Composition, Major, OriginalCompSeedMayBeFalse}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object MajorQP2 extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	//val calling = "WH BMH WM WHH W"
	val calling = "H WHH WW B B BMWH"
	//val calling = "H WHH WWBHH"
	//val calling = "WH BM'H' W M W' /"

	//val calling = "H WHH WW B MH WM WH"

	//val calling = "WH B M'H'H BMH BH WWW' /"

	//val calling = "HH'H M'W WHH M'H'H' M'W MH' W'WH MMMW'HH'H"

	//val calling = "HH'H M'WWHH WMH' W'WH W'HH'H"

	//val calling = "HH'H M'WWHH WMH' W'WH W'H'"


	val methods = List(lessness, cornwall, superlative, bristol)
	//val methods = List(lessness, cornwall, venusium, superlative, bristol)

	override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	/*
	for (m <- methods)
	{
		println(m.toString+":")
		AnalyseMethods.printBestCourses(m)
		println
	}
	*/

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	def scoreFn(comp: Composition) = ScoreFactory(comp).qpFinderAtw

	override lazy val musicDefs =
	{
		Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens())
	}

	override lazy val calls = List(Major.Bob, Major.Single)

}