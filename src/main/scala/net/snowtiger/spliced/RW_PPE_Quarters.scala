package net.snowtiger.spliced

import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object RW_PPE_Quarters extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	val calling_AGR = "BBBMWHWHHWHHH"
	val calling_MBD1 = "HH BHH BH BWW BHHH BH MM"
	val calling_MBD_AGR1 = "HH BHH W BBWW BHHH BH MM"
	val calling_MBD2 = "WHH MBWWH MWW BHH"
	val calling_MBD_Joker = "WH BH' M'H' M'WWH BH BMW MW'W'H W"
	val calling_MBD3 = "MWH WHH WH MW'H' W MH W"

	val calling = calling_MBD3

	//val methods = List(superlative, lancashire, mareham, cooktownOrchid, deva, glasgow, yorkshire, lessness, cornwall, bristol)
	val methods = List(superlative, kenninghall, mareham, rook, deva, glasgow, yorkshire, lessness, cornwall, bristol)
	//val methods = List(london, superlative, cambridge, yorkshire, lessness, cornwall, bristol)
	//val methods = List(superlative, cambridge, cornwall, lessness)
	//val methods = List(cambridge, cornwall, yorkshire)

	//override val seed = Some("PPE\\core7_gamma.txt")
	def scoreFn(comp: Composition) = ScoreFactory(comp).qpFinder

	override lazy val musicDefs = Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens(), new MusicQueensRow)

	def generate() = tunnel(this)

}