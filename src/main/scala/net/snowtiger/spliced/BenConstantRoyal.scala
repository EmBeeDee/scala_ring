package net.snowtiger.spliced

import net.snowtiger.ringing.{Music, NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

import scala.io.Source

/**
 * @author mark
 */

object BenConstantRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val london = NamedMethod("London", 10, "3x3.4x2x3.4x4.5.6x6.7.4.58.4.9", "12")											// f
	val cambridge = NamedMethod("Cambridge", 10, "x30x14x1250x36x1470x58x16x70x18x90", "12")						// b
	val sgurr = NamedMethod("G Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val pudsey = NamedMethod("Pudsey", 10, "x5x6x27x38x4x5x6x7x8x9", "12")															// b
	val lincolnshire = NamedMethod("N Lincolnshire", 10, "x3x4x5x6x7x8x6x7x58x9", "12" )								// b
	val superlative = NamedMethod("Superlative No.2", 10, "x38x478x7x38x47x38x4x347x38x9", "12")			// b

	val calling1 = "MH W W MH M MH H MW MH WH MW W M MWH" // Pooley
	val calling2 = "WWWH W MWWHH WHHH MW MWH W MMMWWHH WHH" // Kippin
	val calling3 = "MH W W MH S'W' S'W' M MH H MW MH WH MW W M MWH" // Pooley 2
	val calling4 = "H WHH MMH W MWHH W WH"	// Reading 1
	val calling5 = "H WHH MMH W MWHH WHHH WH" // Reading 2 / Robin
	val calling6 = "W'H'HH' M'H MWW MW' H'HMH"	// Hull 1
	val calling7 = "MH WWHHH' M'H WHH' M' WM"		// Hull 2
	val calling8 = "MH WWHHH' M'H' W'HH' M' WM"	// Hull 3
	val calling9 = "MH WWHH MHH WHH' M' WM" // Hull 4
	val calling10 = "MH WWHH MHH WH MH WM" // Hull 4a
	val calling11 = "MH WWMMHH MMH W"	// Hull 5
	val calling12 = "MH WWMH' M'H MMH W"	// Hull 5a
	val calling13 = "WHH WW MMH WMH W"		// Davies 1 = Hull 5 reversed
	val calling14 = "HH WWH WWMMH WMH W"		// Davies 2
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
	val calling = calling_t4

	val methods = List(sgurr, cambridge, superlative, triton, bristol)

	//override val seed = Some("BenConstant/BCLS_seed.txt")
	//override val seed = Some("BenConstant/BLSY_t4.txt")
	//override val seed = Some("BenConstant/BLSY_14.txt")
	//override val seed = Some("BenConstant/pretty.txt")

	//override val seed: Option[String] = Some("T 5120 5-Spliced (Score=5018881, COM=110, LR=2, ATW=405, music=313/14/6/1/76/5, LWM=38)  SCTSCTCS-T- BCGB-SCTSS-B CG-C-GGT-BSCTCS T-CBBSBB-GBSC- GGT- SG-TGBG BBG-GTTGB- BGSC-T B(28/74) C(24/34) G(28/101) S(24/49) T(24/55)  (LTLM=11, LA=21, homes=8) ATW")

	//def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)
	def scoreFn(comp: Composition) = atwScore(comp)

	//def generate() = prettyPrint()
	def generate() = tunnel(this)
	//def generate() = new MultiMethodGenerator(this, extraMethods).multi(400000)

	val methodAssessor = MethodAssessor(10)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*40 +
				comp.atwScore*30 +
				ScoreFactory.balanceScore(comp)*10 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*30 +
				(if (comp.isAtw) 50 else 0) +
				comp.music(0)*2+comp.music(1)+comp.music(2)*6+comp.music(3)*10

	def balanceScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*2 +
				ScoreFactory.balanceScore(comp)*40 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*3 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+comp.music(1)

	lazy val extraMethods = methodAssessor.parseMethods(Source.fromFile("TDRoyal.csv")).
			filter{methodAssessor.isGoodRoyal(_)}.
			filter{methodAssessor.hasNewWork(methods)}
			//filter{methodAssessor.hasNewStart(methods)}

	override lazy val calls = List(Royal.Bob, Royal.Single, Royal.BigBob)

	val front7890 = new Music()
	{
		override def countMusic(row: Row) =
		{
			val n = row.nbells
			var m = 0
			if (row.bellAt(1)==7 && ascendingRun(row, 1, n-6))
				m+= 1
			m

		}
	}
	val backRounds = new Music()
	{
		override def countMusic(row: Row) = if (row.reverse.isRounds) 1 else 0
	}
	override lazy val musicDefs = Array(new StandardMaxMusic(), new Music56Rollup(), front7890, backRounds, new MusicLB(5), new Music65Rollup())

}