package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

import scala.io.Source

/**
 * @author mark
 */

object TrickyOnes extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val glasgow = NamedMethod("Glasgow", 8, "36-56.14.58-58.36-14-38.16-16.38","18")			// g, BE
	val london = NamedMethod("London", 8, "38-38.14-12-38.14-14.58.16-16.58", "12" )		// f, BD
	val belfast = NamedMethod("F Belfast", 8, "34-58.14-12-38.12-14.38.16-12.38", "18")		// m, BDE
	val chomolungma = NamedMethod("Chomolungma", 8, "38-38.16-12.56.38.12-14.38.12-16.38", "12")	// b, BDEK
	val whitemoor = NamedMethod("Whitemoor", 8, "x34x14.58x12.38.14x14.58x14.36.58", "18")	// m, BDK

	// 4800 Belfast seed
	//val calling = "BH MB MBBBBW BWH MW B MWW MMW B MH MWH W B MH MW"
	// 5056 Chomolungma seed
	//val calling = "WBMHH WBMHH WBM BBH BBH BBH BBH BB WBMHH WBMHH"
	// 5120 Chomolungma seed
	val calling = "WBBH M BMHH WBMHH WB WHHH WBH BBH BBH BMH WBMHH"
	// 5120 Glasgow seed
	//val calling = "BH MH BBBBBH MH BBBHHH BBH MH BBBHHH B"
	// 5120 London seed
	//val calling = "MBWH MW MWH MBWH BH MWH WH MH MWH BWH MWH MB MBBWW MWWHHH MH BWW MWHWH"

	// DFM 5-part seed
	//val calling = "MMWWH MBBWWH B MH' WH' MMWWH MBBWWH B MH' WH' MMWWH MBBWWH B MH' WH' MMWWH MBBWWH B MH' WH' MMWWH MBBWWH B MH' WH'"

	//override val seed = Some("davies5\\dfm.txt")

	val methods = List(whitemoor,glasgow,belfast,chomolungma)

	override lazy val calls = List(Major.Bob, Major.Single)

	def scoreFn(comp: Composition) = atwScore(comp)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*1 +
				comp.atwScore*20 +
				ScoreFactory.balanceScore(comp)*50 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*5 +
				(if (comp.isAtw) 50 else 0) +
				comp.music(0)+comp.music(1)*2

	val methodAssessor = MethodAssessor(8)

	val extraMethods = methodAssessor.parseMethods(Source.fromFile("surprise.csv")).
			filter(methodAssessor.isTricky).filter(_.isPlainBobType).
			filter(methodAssessor.hasNewLHGroup(methods)).
			filter(methodAssessor.hasNewStart(methods)).
			filter(methodAssessor.hasNewWork(methods))

	//def generate() = new MultiMethodGenerator(this, extraMethods).multi(100000)
	def generate() = tunnel(this)
}