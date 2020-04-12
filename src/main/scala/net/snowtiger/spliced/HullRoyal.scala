package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

import scala.io.Source

/**
 * @author mark
 */

object HullRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val precambrian = NamedMethod("Precambrian", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "12")		// f
	val pangaea = NamedMethod("p Pangaea", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "10")					// l
	val sgurr = NamedMethod("Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val small = NamedMethod("@ Small's Friends", 10, "x50x14.50x12.30x3478x58x16x70.16x18.90", "10")		// k1
	val eagle = NamedMethod("Eagle Nebula", 10, "34x30.16x1270.58.16x78x50.16x16.70.16x16.70", "10")		// l
	val pollock = NamedMethod("Pollockshields", 10, "36x56.14.50x50.16x14x50.14x14.30x12x10", "10")			// m
	val proteus = NamedMethod("U Proteus", 10, "34x50.14x50x30x14x50.14x14.70.16x18.90", "12")					// c1
	val galatea = NamedMethod("Galatea", 10, "34x50.16x56x10x14x50.14x14.70.16x18.90", "10")						// k1

	 val copperdragon = NamedMethod("Copper Dragon", 10, "x5x4.5x5.36x4x58.4x4.7.6x78.1", "10")					// g
	val goldfinger = NamedMethod("Goldfinger", 10, "3x5.4x5x36.2x7.58.6x6.7x6x1", "12")									// b
	val harrison = NamedMethod("Harrison", 10, "x30x14x12.50.16x34x50x16x70.16x56.70", "12")						// f
	val independence = NamedMethod("Independence Day", 10, "30x50.14x50x30x34x50.16x16.70x16x70", "12")	// f
	val ingoldsby = NamedMethod("i Ingoldsby", 10, "x30x14x56x36.14x34.50.16x16.70x16x70", "10")				// k1
	val ireby = NamedMethod("! Ireby", 10, "x34x4x2x3.2x4.5.6x6.7x6x7", "10")														// k1
	 val johnby = NamedMethod("Johnby", 10, "x50x14.50x12.30x12x18x16x70.12x16.30", "10")								// j1
	val newJ1 = NamedMethod("j New J1", 10, "-50-14.50-12.30-12-10-16-70.1258.34-10", "10")							// j1
	 val knifesmithgate = NamedMethod("Knifesmithsgate", 10, "x5x4.5x5.8.7x4.3.6x6.7x6x1", "10")				// m
	val lochdubh = NamedMethod("Lochdubh", 10, "x3x4x2.5.6x34.7.58.6x6.7x6x1", "10")										// m
	val nobottle = NamedMethod("Nobottle", 10, "3x5.4x2x36.7x4.58.6x6.7x6x9", "12")											// d1
	val raspberry = NamedMethod("Raspberry Crumble", 10, "30x50.14x50x30x12x18x56.14.30.12x18.90", "12") // f
	 val ringingchat = NamedMethod("Ringing Chat", 10, "x50x14.50x12.36.70.34x10x16x70.16x18.90", "10")	// l
	val stanage = NamedMethod("s Stanage Edge", 10, "34x30.14x12x30x14x50.16x16.70x16x70", "10")				// l
	 val stmary = NamedMethod("St Mary Abbots", 10, "x5x4.5x2.3x2x1x6x5x6x5", "12")											// b
	val usselby = NamedMethod("Usselby", 10, "34x5.4x2x3x2x5.6x6.7x6x7", "10")													// l
	val xapuri = NamedMethod("Xapuri", 10, "34x50.14x12x30.14x70.18.36x16.70x16x10", "10")							// l
	 val zamara = NamedMethod("Zamara", 10, "x50x14.50x12.36x12.70.58.16x16.70x16x10", "10")						// m

	val methods = List(galatea, sgurr, triton, eagle, proteus, bristol)
	//val methods = List(newJ1, sgurr, raspberry, triton, bristol)
	//val methods = List(raspberry, nobottle, sgurr, triton, bristol)
	//val methods = List(stmary, raspberry, nobottle, triton, bristol)

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
	val calling11onePart = "MH WWMMHH MMH W MH WWMMHH MMH W"	// Hull 5 one-part
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
	val calling = calling11//onePart

	//override val seed = Some("HullRoyal/pretty.txt")
	//override val seed = Some("HullRoyal/seedGalatea.txt")

	//def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)
	def scoreFn(comp: Composition) = atwScore(comp)
	//def scoreFn(comp: Composition) = musicScore(comp)

	//def generate() = prettyPrint()
	def generate() = tunnel(this)
	//def generate() = onenudge(this)
	//def generate() = new MultiMethodGenerator(this, extraMethods).multi(400000)

	val methodAssessor = MethodAssessor(10)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*40 +
				ScoreFactory.balanceScore(comp)*2 +
				(ScoreFactory.strictLenScore(comp)*(2+(if (comp.isAtw) 1 else 0))) -
				comp.longestNoComRun*5 +
				(if (comp.isAtw) 50 + comp.music(0)*2+comp.music(1) else 0)
	//2000/(comp.falseScore+1) - 10*comp.falseScore

	def musicScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*20 +
				ScoreFactory.balanceScore(comp)*5 +
				(ScoreFactory.strictLenScore(comp)*(2+(if (comp.isAtw) 1 else 0))) -
				comp.longestNoComRun*5 +
				comp.music(0)*2+comp.music(1)

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
	//val compositeMusicDef = new CompositeMusic(new MusicRun(4), new MusicRun(5), new MusicRun(6), new MusicAscendingRunTenorBehind(4,2), new MusicAscendingRunTenorBehind(5,2), new Music4Course(), new Music56Rollup(), new Music65Rollup(), new MusicTittumsRow())
	//override lazy val musicDefs = Array(compositeMusicDef, new Music56Rollup(), new MusicTittumsRow(), new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music4Course())


}