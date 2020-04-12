package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, Music, NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.Node

/**
 * @author mark
 */

object GunningMax extends SplicedGenerator with SearchDefinitionBase
{
	val ariel = NamedMethod("a Ariel", 12, "x5Tx14.5Tx12.3T.14x12.5T.14x369T.70.18x18.9T.18x18.ET", "1T")			// k1
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val bowyer = NamedMethod("b Bowyer", 12, "3Tx3T.14x12x3T.14x14.5T.14x36.7T.18x18.9Tx18x9T", "12")					// f
	var barford = NamedMethod("b Barford", 12, "3x3.4x2x3.4x4.5.6x6.7.58x6.9.6x6.9", "12")										// f
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val snowtiger = NamedMethod("Snow Tiger", 12, "3Tx5T.14x5Tx3Tx12x1TxETx10x18x9T.18x10.ET", "12")					// f
	val parsons = NamedMethod("p Parsons Pleasure", 12, "3Tx5T.14x5Tx3Tx34x5Tx16x7T.16x16.9T.18x10.ET", "12")	// d2
	val phobos = NamedMethod("Phobos", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "1T")							// l
	val rigel = NamedMethod("Rigel", 12, "36x7T.18x9T.50.36.14x1470.5T.14.36.9T.10.58x16.7T.16.70.16.ET", "1T")	// l
	val yorkshire = NamedMethod("Yorkshire", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18xET", "12")						// b
	val cambridge = NamedMethod("Cambridge", 12, "x3x4x25x36x47x58x69x70x8x9x0xE", "12")											// b
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")				// j2

	//val methods = List(ariel, zanussi, bristol, phobos, yorkshire)
	//val methods = List(avon, zanussi, phobos, bristol, yorkshire, snowtiger)
	val methods = List(avon, phobos, snowtiger, zanussi, bristol)
	//val methods = List(ariel, phobos, rigel, zanussi, bristol)
	//val methods = List(avon, zanussi, snowtiger, rigel, phobos, bristol)
	//val methods = List(ariel, zanussi, rigel, phobos, bristol, snowtiger)

	//val methods = List(avon, phobos, zanussi, bristol)

	val calling1 = "H W MMH MWH H H MWH WW MH" // Byrne
	val calling2 = "H WM MH MW MH WWHH MH " // Davies1
	val calling2a = "H WHHH M MH MW MH WWHH MH " // Davies1a
	val calling3 = "W'H' MMH MW MH WWHH M'H' " // Davies2
	val calling4 = "H WMMM WH WMMH WMH WM WH " // Davies3
	val calling5 = "MHH' M'W'H'H WH' M'W MWHH MW MW'H'"	// Davies4

	val callingAGR1 = "WH MMWWHH MMH' W'HH MH'H' MH WHHH"	// A G Reading 1

	override val calling: String = calling2a

	override val seed = Some("JackGunning/3_ABPSZ.txt")
	//override val seed = Some("T 5088 5-Spliced (Score=5001039, COM=82, LR=3, ATW=483, music=498/2/10/29/314/139/6, LWM=22)  ZABABZZZAS- AAAZS-BBPZ- BBPBB- A- S-SZPA S-BZSPAZB A- BBPZ-ABZPAAS-ZPPZPPZB P-SPSPASSS- ABZAPAZB-S SSPSPPS-ZPPP A- BZZZPZ- BZPPZ-SBBABAZ- A(20/66) B(21/82) P(23/168) S(18/72) Z(24/110)  (LTLM=17, LA=17, homes=14)")

	//def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)
	//def scoreFn(comp: Composition) = atwScore(comp)
	def scoreFn(comp: Composition) = musicScore(comp)

	def generate() = prettyPrint()
	//def generate() = tunnel(this)
	//def generate() = onenudge(this)
	//def generate() = new MultiMethodGenerator(this, extraMethods).multi(400000)

	def musicScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				ScoreFactory.balanceScore(comp)*2 +
				ScoreFactory.strictLenScore(comp) -
				comp.leadsWithoutMusic*50 -
				comp.longestAbsence*50 -
				comp.longestNoComRun*200 +
				comp.music(0) + comp.music(1)*30 + comp.music(2)*20 + comp.music(3)*2

	val compositeMusicDef = new CompositeMusic(new MusicLB(4), new MusicLB(5), new Music56Rollup(), new Music65Rollup(), new Music7890ETOffFront())
	override lazy val musicDefs = Array(compositeMusicDef, new MusicRun(12), new Music7890ETOffFront(), new Music56Rollup(), new MusicLB(4), new MusicLB(5), new Music65Rollup())

	/*
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		if (inSeed)
			true
		else if (!trickyNode(node))
			true
		else if (goodNode(node))
		{
			println("Yes: "+node)
			true
		}
		else
			false
	}
	*/

	private def trickyNode(node: Node) =  Set("1423567890ET", "123648507T9E").map(Row(_)).contains(node.startLH)


	private def goodNode(node: Node) = node.leadsWithoutMusic==0

	class Music7890ETOffFront extends Music
	{
		def countMusic(row: Row) =
		{
			val n = row.nbells
			var m = 0
			if (row.bellAt(1)==7 && ascendingRun(row, 1, 6))
				m+= 1
			m
		}

	}


}