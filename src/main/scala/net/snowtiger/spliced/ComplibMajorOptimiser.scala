package net.snowtiger.spliced

import net.snowtiger.ringing.CompositeMusic
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * Optimiser for various existing Major compositions, in an attempt to improve CompLib scores.
 *
 * @author mark
 */
object ComplibMajorOptimiser extends SplicedGenerator with SearchDefinitionBase with StandardMethods
{
	val callingPitman4Spliced1 = "MMWW MH MWWBH WHH MWWWHH MWWWH WHHH MH MMMWHH MMMWHH MH B MMWH WHH WH MMMH MMWWH WWWH MMH BH BH WH"
	val callingPitman4Spliced2 = "H MH MMMWHH WHH MH BMMWH WHH WH MMMH MMWWH WWWH MMH BH BH WH MMWW MH MWW BH WHH MHH MWWWH WHH"

	val methods = List(bristol, cambridge, london, superlative)

	val calling = callingPitman4Spliced2

	def generate() = anneal(this)

	val complibMusic = new CompositeMusic(new MusicRun(4), new MusicRun(5), new MusicRun(6), new MusicRun(7), new Music56Rollup(), new MusicCRU(), new MusicBack4Combinations())
	override lazy val musicDefs = Array(complibMusic, new MusicRun(4), new MusicRun(5), new MusicRun(5), new MusicRun(6), new MusicRun(7), new Music56Rollup(), new MusicCRU(), new MusicBack4Combinations())

	override def scoreFn(comp: Composition) = score1(comp)

	def score1(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*80 +
				ScoreFactory.balanceScore2(comp)*2 +
				2*ScoreFactory.strictLenScore(comp, 0.2) -
				comp.falseScore*75 -
				comp.longestNoComRun*100 + comp.com*1 +
				comp.music(0)+comp.music(1)+comp.music(6) //2*comp.music(0)+2*comp.music(1)+comp.music(2)+5*comp.music(3)+comp.music(4)+10*comp.music(5)

}
