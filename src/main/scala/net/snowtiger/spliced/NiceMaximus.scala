package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.Splice

/**
 * @author mark
 */

object NiceMaximus extends SplicedGenerator with SearchDefinitionBase
{
	val miranda = NamedMethod("Miranda", 12, "34-5T.14-5T.12.3T.14-14.5T.36-9T.70.18-18.9T.18-18.ET", "1T")		// This is "unrung" Surprise, not the Delight
	val strathclyde = NamedMethod("Strathclyde", 12, "36x56.14.5Tx5T.36x14x3T.16x16.3T.16x16.3T.16x16.3T", "1T")	// k

	val ariel = NamedMethod("a Ariel", 12, "x5Tx14.5Tx12.3T.14x12.5T.14x369T.70.18x18.9T.18x18.ET", "1T")			// k1
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val bowyer = NamedMethod("b Bowyer", 12, "3Tx3T.14x12x3T.14x14.5T.14x36.7T.18x18.9Tx18x9T", "12")					// f
	var barford = NamedMethod("b Barford", 12, "3x3.4x2x3.4x4.5.6x6.7.58x6.9.6x6.9", "12")										// f
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val snowtiger = NamedMethod("Snow Tiger", 12, "3Tx5T.14x5Tx3Tx12x1TxETx10x18x9T.18x10.ET", "12")					// f
	val parsons = NamedMethod("p Parsons Pleasure", 12, "3Tx5T.14x5Tx3Tx34x5Tx16x7T.16x16.9T.18x10.ET", "12")	// d2
	val phobos = NamedMethod("Phobos", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "1T")							// l
	val yorkshire = NamedMethod("Yorkshire", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18xET", "12")						// b
	val cambridge = NamedMethod("Cambridge", 12, "x3x4x25x36x47x58x69x70x8x9x0xE", "12")											// b
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")				// j2

	/*
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
		"WH M MW MWWH"		// 12 471 * 469
	)
	*/

	val callings = Array(
		"MWH MH W",			// 0
		"MWH MH MMMW",	// 1
		"MWH MH WHHH",	// 2
		"MWH MWWWH W",	// 3
		"H WWH H H MWH",	// 4
		"MWH MMW MMW",	// 5
		"WW MH MWH",		// 6
		"W MW MMWH",		// 7
		"W MWWH MWH",		// 8
		"H MWW MWH WH"	// 9
	)

	//val calling = callings(7)*2
	//val calling = "MHH' M'W'H'H WH' M'W MWHH MW MW'H'"	// touch A
	//val calling = "W'H' MMH MW MH WWHH M'H'"						// touch B
	//val calling = "MWH NN'W' M'H'H MW MH W"							// touch C
	//val calling = "MWH NN'W' M'H'H MW MW' H'"							// touch C2
	//val calling = "WH W MMWH WWH NN'W' M'H'H MW MW'H'"		// touch D

	val calling6 = "W'H'HH' M'H MWW MW' H'HMH"	// Hull 1
	val calling7 = "MH WWHHH' M'H WHH' M' WM"		// Hull 2
	val calling8 = "MH WWHHH' M'H' W'HH' M' WM"	// Hull 3
	val calling9 = "MH WWHH MHH WHH' M' WM" // Hull 4
	val calling10 = "MH WWHH MHH WH MH WM" // Hull 4a
	val calling11 = "MH WWMMHH MMH W"	// Hull 5
	val calling12 = "MH WWMH' M'H MMH W"	// Hull 5a
	val calling13 = "WHH WW MMH WMH W"		// Davies 1 = Hull 5 reversed
	val calling14 = "HH WWH WWMMH WMH W"		// Davies 2

	val calling15 = "H H MWM M MWH M"
	val calling16 = "H W MMMW MW WH"

	val warboysCalling = "WWH MW MH MMWHH"
	val constantCalling = "H MWW MH MMWHH"
	val oliverAustinCalling = "H W MMMW MWW H"

	//val calling = warboysCalling
	//val calling = constantCalling*2
	//val calling = oliverAustinCalling
	val calling = calling13*2//  +"/1654327890ET "+calling13

	//val methods = List(parsons, avon, yorkshire, zanussi, bristol)
	//val methods = List(snowtiger, avon, yorkshire, zanussi, bristol)
	//val methods = List(snowtiger, phobos, yorkshire, zanussi, bristol)
	//val methods = List(snowtiger, phobos, ariel, zanussi, bristol)
	//val methods = List(snowtiger, yorkshire, ariel, zanussi, bristol)
	//val methods = List(snowtiger, ariel, zanussi, bristol)
	//val methods = List(snowtiger, avon, zanussi, bristol)
	//val methods = List(miranda, zanussi, avon, phobos)
	val methods = List(cambridge, zanussi, bristol, barford)

	override val seed = Some("maximus/Constant4d.txt")

	//override def makePlan = new CompositionPlan(calling, List(CallingPosition.Bob, CallingPosition.Single), getCallingMethod)
	//override lazy val calls = List(Maximus.Bob, Maximus.Single)

	//def scoreFn(comp: Composition) = ScoreFactory.strictAtwFinder(comp)
	def scoreFn(comp: Composition) = ScoreFactory.levenshteinPartMusicFinder(comp)

	//def generate() = onenudge(this)
	//def generate() = tunnel(this)
	def generate() = prettyPrint()

	override def acceptSplice(splice: Splice) = splice.methods.size<5 || splice.com<=0

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	/*
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val wholeCourseCOs = Set("53246", "35642", "64235", "46532")
		if (wholeCourseCOs.contains(node.startLH.coursingOrder(bristol.plainPerm)))
			node.methodsUsed.size==1
		else
			true
	}
	*/
}