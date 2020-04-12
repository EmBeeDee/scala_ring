package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.Splice

/**
 * Spliced Max for David Hull
 *
 * @author mark
 */
object HullMax extends SplicedGenerator with SearchDefinitionBase
{
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val azura = NamedMethod("U Azura", 12, "-5T-14.5T-12.3T-12-1T-16-7T.16-16.7T.16-16.7T", "1T")		// k1
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val counters = NamedMethod("Counter's Creek", 12, "x5x4.5x2.3x2x1x6x7.6x6.7x6x7", "1T")	// k1
	val deimos = NamedMethod("Deimos", 12, "34-5T.16-56-36.7T.34-1T-70.18.3T.10.3T", "1T")	//g
	val euximoor = NamedMethod("Euximoor Fen", 12, "-5T-14.5T-7T.36-7T-58-16-70.16-18.9T-18-1T", "1T")	// k
	val fenchurch = NamedMethod("Fenchurch", 12, "3T-3T.14-12-3T.14-14.3T.16-14.3T.18.36.18.9T.18-18.9T", "12")	// f
	val jannu = NamedMethod("j Jannu", 12, "36-56.14.5T-5T.36.12-147T.58.16-16.70-16-9T-12.30.1T", "1T")	// j2
	val jabberwock = NamedMethod("Jabberwock", 12, "36-56.14.5T-5T.16-78-18-16.9T.70.16-18.9T-18-ET", "1T")	// j2
	val mottram = NamedMethod("Mottram", 12, "34-5T.14-5T-3T.14-14.5T.36-16.7T.58-18.9T.70-10.9T", "1T") // g
	val phobos = NamedMethod("Phobos", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "1T") // l
	val palatino = NamedMethod("T Palatino", 12, "-5T-14.5T-12.3T-34-5T.16-9T.70.18-18.9T.18-18.ET", "1T") // k1
	val rigel = NamedMethod("Rigel", 12, "36x7T.18x9T.50.36.14x1470.5T.14.36.9T.10.58x16.7T.16.70.16.ET", "1T")	// l
	val roaring = NamedMethod("Roaring Meg", 12, "-5T-14.5T-5T.36.14-14.3T.12-16.7T.18-18.9T-18-9T", "1T")	// l
	val scarlet = NamedMethod("Scarlet", 12, "-5T-14.5T-125T.36.14-7T.58.16-169T.70.58-18.9T-18-ET", "1T")	// j1
	val strawberry = NamedMethod("W Strawberry", 12, "36x56.4.5x5.6x34x25x6x7.6x6.7.6x6.7", "1T")	// h
	val strathclyde = NamedMethod("s Strathclyde", 12, "36x56.14.5Tx5T.36x14x3T.16x16.3T.16x16.3T.16x16.3T", "1T")	// k
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")	// j2

	val neptune = NamedMethod("Neptune", 12, "3x5.4x5x36.2x7.58.6x2.7.2x8.9x8x9", "12")	// c
	val parsons = NamedMethod("p Parsons Pleasure", 12, "3Tx5T.14x5Tx3Tx34x5Tx16x7T.16x16.9T.18x10.ET", "12") // d2

	val new1 = NamedMethod("X Hull New One", 12, "3-5.4-5-3-34-5.6-6.7-6-9.8-0.E", "12")
	val new2 = NamedMethod("Y Hull New Two", 12, "-3-4-2.5.6.34-7.58.6-6.7-6-1-0-9", "1T")

	//val calling = "WH MMWWHH MMW M"
	val calling = "WH MMWWHH MMW M  WH MMWWHH MMW M"

	val methods = List(new1, new2, rigel, bristol)

	def generate() = tunnel(this)
	//override val seed = Some("HullMax/RXYZ3.txt")
	//def generate() = prettyPrint()


	override lazy val musicDefs = Array(new MusicLB(4), new MusicLB(5), new MusicLB(6), new Music56Rollup(), new Music65Rollup())


	override def scoreFn(comp: Composition) = scoreFn1Part(comp)

	def scoreFn2Part(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*80 +
				ScoreFactory.balanceScore2(comp)*2 +
				2*ScoreFactory.strictLenScore(comp, 0.2) -
				comp.falseScore*40 -
				comp.longestNoComRun*8 + comp.com*1 +
				comp.music(0)*2+comp.music(1)+comp.music(2)+comp.music(3)*2+comp.music(4)

	def scoreFn1Part(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*80 +
				ScoreFactory.balanceScore2(comp)*2 +
				2*ScoreFactory.strictLenScore(comp, 0.2) -
				15*comp.levenshteinPartDistance -
				comp.falseScore*40 -
				comp.longestNoComRun*8 + comp.com*1 +
				comp.music(0)*2+comp.music(1)+comp.music(2)+comp.music(3)*2+comp.music(4)

	override def acceptSplice(splice: Splice) =
	{
		true
		//splice.isInSingleMethodComp(getCallingMethod) || splice.length<5 || splice.methodsUsed.size>2
	}
}
