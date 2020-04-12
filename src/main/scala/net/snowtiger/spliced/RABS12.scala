package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object RABS12 extends SplicedGenerator with SearchDefinitionBase
{
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val strathclyde = NamedMethod("Strathclyde", 12, "36x56.14.5Tx5T.36x14x3T.16x16.3T.16x16.3T.16x16.3T", "1T")	// k
	val rigel = NamedMethod("Rigel", 12, "36x7T.18x9T.50.36.14x1470.5T.14.36.9T.10.58x16.7T.16.70.16.ET", "1T")	// l
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")	// j2

	val callings = Array(
		"MWH W MW M",			// 0  469
		"H M H W MWH",		// 1  471 *  472
		"M H W MWH",			// 2  463
		"MWWWH W MWH",		// 3  455
		"MWH WW M H",			// 4  471 ** 475
		"WWH M MWWH H", 	// 5  471 ** 473
		"M H WH H H MWH",	// 6  461
		"WH H M MWWH H",	// 7  464
		"MW M H WH H",		// 8  475 ** 469
		"WH H MWH H MWH",	// 9  460
		"H MWH H WH M H", // 10 465
		"MWH H WH M H",		// 11 463
		"WH M MW MWWH",		// 12 471 * 469
		"M H H MWH H H M",// 13 454
		"H H M MW MWH",		// 14 452
		"H M MWWH H M H",	// 15 466
		"H H M MWWH M M",	// 16 461 *
		"M H H MW M",			// 17 455
		"H MW MWWH H",		// 18 458
		"MWWH H MWH H H",	// 19 461
		"MWH H H MWWH",		// 20 454
		"H MWH H M MWH H",// 21 462
		"H MWH H MWH H M",// 22 459
		"M M H MW M H",		// 23 464
		"M H M M MW MWH",	// 24 455
		"M MW M MWH M H",	// 25 466
		"M H M MW MWWH",	// 26 469
		"M MW MWH H",			// 27 464
		"M MWWH M M H",		// 28 461 *
		"MWH H M MWH H",	// 29 465
		"M MWH MWW M H",	// 30 463
		"MW M H MWH M H",	// 31 467
		"M MW MWH H MW",	// 32 463
		"MWWWH H MW M",		// 33 461
		"H MW MW MWH M",	// 34 463
		"H MW MWWH",			// 35 460
		"H MWWH H MWH",		// 36 459
		"MWH H MWH M M H",// 37 464
		"M MWH MWWH H MWH",//38 466
		"MW MW MWH M",		// 39 464
		"MWW MWW M",			// 40 456
		"M MWH MW MWWH",	// 41 463
		"MW MWWH MW",			// 42 463
		"MW MWWH",				// 43 450

		"MM'W' MW' H'"		// 44 457
	)

	//val calling = "H MWW MH WH MH"							// RAP
	//val calling = "H MWH MH' M'W MWHH MH"							// MBD1
	//val calling = "MH W MWH"							// MBD1
	val compNum = 4
	println("COMP "+compNum)

	/*
	//val methods = List(avon, rigel, strathclyde, bristol)
	//val methods = List(avon, zanussi, strathclyde, bristol)
	val methods = List(avon, rigel, zanussi, bristol)

	val calling = callings(compNum) *2
	//val seed = Some("maxRABS/rabz_1nudge3.txt")
	val seed = Some("maxRABS/rabz_atw.txt")

	val scoreFn = ScoreFactory.levenshteinPartMusicFinder

	//def generate() = onenudge(this)
	def generate() = tunnel(this)
	*/

	///*
	//val methods = List(avon, zanussi, strathclyde, bristol)
	//val methods = List(rigel, strathclyde, bristol, avon)
	//val methods = List(avon, rigel, strathclyde, bristol)
	val methods = List(avon, rigel, zanussi, bristol)

	val calling = callings(compNum)

	def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)

	def generate() = tunnel(this)
	//*/

}
