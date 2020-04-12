package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object KippinRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val superlative = NamedMethod("Superlative No.2", 10, "x38x1478x70x38x1470x38x14x3470x38x90", "12")	// b
	val cambridge = NamedMethod("Cambridge", 10, "x30x14x1250x36x1470x58x16x70x18x90", "12")						// b
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b

	//val methods = List(superlative, yorkshire, triton, bristol)
	//val methods = List(superlative, cambridge, bristol, triton)
	//val methods = List(cambridge, bristol, triton, superlative)

	//val methods = List(cambridge, triton, bristol)
	val methods = List(superlative, cambridge, bristol, triton)

	val calling1 = "MH W W MH M MH H MW MH WH MW W M MWH" // Pooley
	val calling2 = "WWWH W MWWHH WHHH MW MWH W MMMWWHH WHH" // Kippin
	val calling3 = "MH W W MH S'W' S'W' M MH H MW MH WH MW W M MWH" // Pooley 2
	val calling4 = "H WHH MMH W MWHH W WH"	// Reading 1
	val calling5 = "H WHH MMH W MWHH WHHH WH" // Reading 2 / Robin
	val calling_b1 = "W MW MMWH W MMW MHH"
	val calling_b2 = "H WWH MMWWW MW MW M"
	val calling_b3 = "WW MH WWH MMW MHH"
	val calling_s4 = "H W MMH MMW MMW MWW MW M"
	val calling_t1 = "WHH MWHH W MMHH MMH W" // ***
	val calling_t2 = "MH MMW MWWW MWHHH MW M"
	val calling_t3 = "MH MMH W MWW MWHHH MW M"
	val calling_t4 = "WH W MMWW MMHH MMH W" // ***
	val calling_t5 = "WH W MWHH MW MMHH MMH W"
	val calling_s1 = "MWWH WW MMMWH W MH MW"
	val calling_s2 = "WWHH MMH MMW MWHHH"
	val calling_s3 = "HH MH W MW MWH WWHH WH"
	val calling = calling4
	//val calling = calling_t4

	override val seed = Some("kippinRoyal/pretty.txt")

	def scoreFn(comp: Composition) = ScoreFactory.musicFinder(comp)

	//def generate() = tunnel(this)


	def generate() = prettyPrint()

	override lazy val calls = List(Royal.Bob, Royal.Single, Royal.BigBob)

}