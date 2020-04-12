package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * Optimiser for various existing Maximus compositions, in an attempt to improve CompLib scores.
 *
 * @author mark
 */
object ComplibMaxOptimiser extends SplicedGenerator with SearchDefinitionBase
{
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val strathclyde = NamedMethod("Strathclyde", 12, "36x56.14.5Tx5T.36x14x3T.16x16.3T.16x16.3T.16x16.3T", "1T")	// k
	val rigel = NamedMethod("Rigel", 12, "36x7T.18x9T.50.36.14x1470.5T.14.36.9T.10.58x16.7T.16.70.16.ET", "1T")	// l
	val orion = NamedMethod("Orion", 12, "36-7T.18-9T.50.36.14-1470.5T.16-9T.30.18-14.3T.50.14-1T", "1T")			// mx
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")	// j2
	val littleport = NamedMethod("Littleport", 12, "-5T-14.5T-5T.36.14-14.5T-14-18", "1T")							// mx

	val callingReading3Spliced = "WHH WWHH W' M'WWHH MMH MWBF'I'B MWWHH"

	val methods = List(littleport, zanussi, bristol)

	val calling = callingReading3Spliced
	//override val seed = Some("AlanReadingORABS/ORABS.txt")

	def generate() = tunnel(this)

	val complibMusic = new CompositeMusic(new MusicRun(4), new MusicRun(5), new MusicRun(6), new MusicRun(7), new MusicRun(8), new MusicRun(9), new MusicRun(10), new MusicRun(11), new Music56Rollup(), new Music65Rollup())
	override lazy val musicDefs = Array(complibMusic, new MusicRun(4), new MusicRun(5), new MusicRun(5), new MusicRun(6), new MusicRun(7), new MusicRun(8), new MusicRun(9), new MusicRun(10), new MusicRun(11), new Music56Rollup(), new Music65Rollup())

	override def scoreFn(comp: Composition) = score1(comp)

	def score1(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*80 +
				ScoreFactory.balanceScore2(comp)*2 +
				2*ScoreFactory.strictLenScore(comp, 0.2) -
				comp.falseScore*40 -
				comp.longestNoComRun*20 + comp.com*1 +
				comp.music(0)+comp.music(1)+2*comp.music(10) //2*comp.music(0)+2*comp.music(1)+comp.music(2)+5*comp.music(3)+comp.music(4)+10*comp.music(5)

}
