package net.snowtiger.spliced

import net.snowtiger.spliced.composition.{Composition, CompositionPlan, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object Standard8 extends SplicedGenerator with SearchDefinitionBase with StandardMethods
{
	val callings = Map(
		"Middletons" -> "MMWWHHH "*5,		// ATW=392, music=127/34/26/3 - seed from Yorkshire
		"GACJ NCR" -> "BBMH WHH WWBH W"*5,
		"Morrison" -> "WM BBH BWH WM BBH BW "*3,	// False with all seeds

		"Seed 191" ->  "B M HW B BW B MW B B B HB MB M HHW B MB HB HB B HHWW B M HW B H",		// ATW, music=186/46/35/13
	 	"Seed 190.2" -> "B M HW B BW B MW B B B HB MB M HHW B MB HB HB B HHMB B M HW B H",
		"Seed 189" -> "B BW B M HB B HW B BW B MW B B B HB MB M HHW B MB HB HB B HHM H",
		"Seed 192.2" -> "B M HW MW B MB HBWW MW BW B MW B B B HB MB M HW B B HHWW B M HB HH",
		"Seed 198.1" -> "MB HW B HB B B B HHWW B MMB MB MB HB B B BW B MMB HHMMB B HHM HMB M HW B H",
		"Seed 198.2" -> "MB HW B HB HB BW B B HHWW B MMB MB MB HB B B BW B MMB HHMB HM HMB M HW B H",
		"Seed 196" -> "M HMBWW B B BW B MMB MB B B HHWW B MMBWW M HW B HB HB B HHM HMB M HW B H",

		"Seed 193.1" -> "B B B MB MMB MB M HW B B HHMB HM HMB M HWW BW B MW B B B B HBWW HW HH",		// ATW, music=197/55/32/8/4
		"Seed 193.2" -> "W B M HHB HB B MW MMB HB B HB HHW B MB B B B B HB MB MB HB B BWW B H",
		"Seed 193.3" -> "HB MW MMB HB B HB HHW B MB B B B B HB MB MB HB B B BW B M HHB HW B H",
		"Seed 192.3" -> "B M HMMB MM HB HW B HHB HHMBWW B B BW B MW B HM HHW B MM HHB M HW B H",	// ATW, 151/34/29/11/0

		"Seed 162 singles" -> "B M HW B BW B H'W'W M HB HHW HB B B B BWW B MM HHB HW B H",			// 392, music=140/32/25/13
		"Bristol 5120" -> "H W BH MBBBBBH B MH BH W B MMH MW MW MMBWW MW MWW MH",		// ATW, music=186/46/35/13
		"Bristol 5088 no.7" -> "H W BH MB MMH BH W BH'H MM'WH BBBWH BW BBBH BHH MBBWW MH",	// ATW=392, music=139/17/31/12
		"Bristol 5120 no.4" -> "BBBBWH MH B MMBH MMH WW BW BBBHH WW' M'MB MMH W MMH MB",	// ATW, music=132/27/29/13
		"Bristol 5152 no.4" -> "BBBBWWH MB MBHH MBH BH BHH B MMB MMHH MH WW BW BBWW BWHH B",	// 390, music = 144/43/22/13
		"Bristol 5152 no.3" -> "BWH WW MHH B MMW MW BB MBBBBBHH MH WW BH MBBBB",		// 392, music=120/31/24/10

		"Seed 1" -> "B B B M HWW B B B MB B HW B BWW HMB MB HB HB B MB MB B MB B M HH ",
		"Seed 2" -> "B B B M HMB B B HB BW BW B B HB MB B MW B B B MB MB B MMW BWW BW ",   //BAD!
		"Seed 8" -> "M HBW HM HMB B HMW B MB B MW MW B B BW B MB BW HB B B BW B M HW ",	//BAD!
		"Seed long" -> "B B B MB HW B HB B B MB B M HB B HB B B B BW B BWW B B B MB B MB B B B HW ",
		"Seed short" -> "B B B M HMW B B HB MB B MW B B B MB MB B MMW BWW HM H",

		"B1" -> "B B W WH B W MW MH B W MH H M MW WH H M M BH H WH M B WH W W BH WH MH W W B",
		"C1" -> "W MWBWH H MH W MW MBMH BH W WBH M MW WH H W W M MH H M MBH M MW WH H ",

		"new1" -> "B M H W B W B H M B B M B H H W W H B W B B B B W H H W",
		"new2" -> "M H W B M W W H M B H B H B M M W W B B W B M H H M H B H W W B",
		"new3" -> "B B M H M B B W W H W B H M M H H M H H W H M W B M M H H M M B H M H B H M M B M B",	// 218 long anneal
		"new4" -> "B M M H H M M B H M H B H M H B W W H W B M H M B M M H H M M B H M H B H M M B M B",	// 208
		"new5" -> "B H B H M H B H M H B W W H W B M H H M H H M H B M M H H M M B H M H B H M M B M B",	// 200
		"new6" -> "B H B H M H W H H M H B W H H W H M H B W W H W B B M H H M M B H M H B H M M B M B",	// 171
		"new7" -> "M H M B M M H H M M H M M B H M H B H M H B W W H W B B M M H B H M H B H M M B M B"		// 196
	)

	val callings44 = List(
		"B B B H M W B W H M H H M M B H M B B M B H W H B B W B M H H M M B H M H B H M M B M B",	// 0 209
		"B B B H M W B W H H B W B H W H M M W M H H W H M B M M B H B B H W B B W W H W W H M B",	// 1 202
		"B B B H M W B W H H B W B H W H M M W M H H W H M W B H H M M B H M B B W W H W W H M B",	// 2 190
		"B B B H M W B H B B M M B H M B B W W H W W M H W H H M B H M H H B W B H W H M M B M B",	// 3 188
		"B B B H M W B H M M B H H M B H W H M H W W B B H W H M M B B B H M B B W W B H H B H B",	// 4 210
		"B B M H B M M H H W H B H W H B W W H M B H H W W B H M M H H W B M H M W B W H H B H B",	// 5 205
		"B M H B W B M H W H M H W W H W B M B H M M B H H M M B H M H B H M H B M W H M M B M B",	// 6 202
		"B M H B W H M B H M H B H M H B W W H W W M H W H H M M B B H H M M B M H W H M M B M B",	// 7 200
		"B H B B M M B B H H B W B W H H B W H B H W H B B W W H M W W M H B H H M W H B W W H W",	// 8 191
		"B H B B M M B B H H W H B H M W B W H H B W H B H W H B B W W H W M M W H B W W M W W B",	// 9 206
		"B H B B M M B H H W M M W H B W W M W M H B H M W B W H M H H M B H W H B B W B M B H B",	// 10 196
		"B H B H B M B B M B H H W B H M W B W H M H W B B W B B W B H B H W W M H B H M M B M B",	// 11 201
		"B H B H B M B B H H W H B H M H B W H H W M H B H W H B H B B W H H B W H B H H B B M B",	// 12 189
		"B H B H B M M B B M W B W W H W W B B H W H M B H H W H B H M W B W H H B W B H W H M W",	// 13 185
		"B H B H B M M H H W H M B H H W H B H M H B W W H W W B H B W H H B W B H W H M M B M B",	// 14 193
		"B H B H M H B H M W B W H M H H M B H W H M H H M W B W B B W B H B H W M H M M W M M H",	// 15 200
		"B H B H M H B H M W B H M B B M W B H B H H M B H W H M H W W H W B H H B W B H W H M W",	// 16 201
		"B H B H M H B H M H B W W H W W M H W H H M M B B H H M H H B W H H B M H W H M M B M B",	// 17 205
		"B H B H M H B H M H B W H M M W H B B H H B W H B H W B M B B B H M M B H H W M H H M B",	// 18 202
		"B H B H M H B H M H B H B H H M H B H M H H W H M H W W H W B H H B W B H W H M M B M B",	// 19 196
		"B H B H M H B H M H B H M B H M H B H M H H B B W B H B H B B W H M H H M B H H B B M B",	// 20 198
		"B H B H M H W H H M B H W H M W M M B H H M H B W W H B B W H H B W B H W H M H W H M B",	// 21 193
		"B H B H M H W H H M B H W H M H W W H W W H M M B B M W M H M W B W H H B W B H W H M W",	// 22 199
		"B H B H M H W H H M B H W H M H H M W M H M W B W H H B W B H W B W H W W H M M B M M W",	// 23 207
		"B H B H W W B W H M M W H B B H H B W H B H W H B B W W H H B H H W H B H M W B H M M H",	// 24 208 !!
		"B H M M B H W H B H M M H H M W B W H M H W B B W B B W B H B H W W H H W M M W M H W B",	// 25 191
		"B H M M B H W H B H M H W H H W M M B H H M H B W B B W B H B H B B W H M H W H M H W B",	// 26 190
		"B H M M B H W H M W B H M H H B W B H W H M M H H M W B H M W M H M B B W W H W W H M B",	// 27 193
		"B H M H H M H H B W H B H W H M H W W H W W H M M W M H M B M H M W B M H W H M M B M B",	// 28 206
		"B H M H H W B B W H M H H M W B W H H B W H B H W H B H W H B H W W H H W M M W M H W B",	// 29 207
		"B H M H H W B M H M W B W H H B W H B H W H M H H W M M W H W B W H W W H M M W M H W B",	// 30 200
		"B H M H H W H M H B M M B B M W M H M W B W H H B W H B H W H M H W W M H W H B W W H W",	// 31 199
		"B H M H H W H M H B M M B H H M H B W H H M H W H B H B B W H H B W H B H W H M H H M B",	// 32 198
		"B H M H H W H M H W W M H M W B W H H B W H B H W H B B W W H H W M M B M W H B W W H W",	// 33 192
		"B H M H H W H M H W W M H M W B W H H B W H B H W H B H W H B W H M M B B H H W W H H W",	// 34 198
		"M W B B H B M H M M B M H B H M H B H M H B W H M B H H W B M H W B H W H B B H H B H B",	// 35 200
		"M W B W W B M W H B H M H B W H M B H B W B H W H B B H H B W H B M H M M B M H B H W B",	// 36 197
		"M W B W H W M M W H B W W B B W H B M W H M M H B B M M B B W B B B M W H B H M H W W B",	// 37 202
		"M W B W H H B W B B B M W H B H M H B W H M M W H B B H H B W H B M H M M B M H B H W B",	// 38 199
		"M W H B B W H B B W B H B W H M B H H M W W M H B H M W M H M B M W H M M H B B B M M W",	// 39 194
		"M W H B H M M H H M H B W B B W B H B H W W H H W M M H M W H M M H M M H M W H M H W B",	// 40 186
		"M W H M B M B M W H H B W B H W H M M W M M B M B B H M W M W B B W W H W W H B B H W B",	// 41 189
		"M W H M B M M B H B B M M H B W B H W H M M W M H M W M M W H B H M H B W W H W W H M B",	// 42 196
		"M W H M B M M B H B M M H M W M M W H W M M H B W B H W H M M H H M H B W W H W W H M B",	// 43 200
		"M W H M B M H M W H W M H B H B M M H M W H H B W B H W H M M H H M H B W W H W W H M B",	// 44 185
		"M W H M B H M B B H M W M W B B W W H W W B M B H B M M H M W H H B W B H W H M M B M B",	// 45 198
		"M W H M B H M H B H B M M W B H M W M W B B W W H W W B B M W H H B W B H W H M M B M B",	// 46 182
		"M W H M B H M H B H M H B H M W M M H H B W B H W H M M B M W B W W H W W B B M H M M H",	// 47 191
		"M W H M W B M M H B W B H W H M M W M H M W M M H M M B H M H B H M H B W W H W W H M B",	// 48 200
		"M W H M W B M H B H M H B H M H B W W B H H B W B H W B B M H M B M H M B M M H W H M B",	// 49 203
		"M W H M W B M H B H M H B H M H B W W B H H B W B H W H M H W W B M H M B M H M B B M B",	// 50 205
		"M W H M W M M W B H M W M H H M H B H M B B W W H W W B B M W H H B W B H W H M M B M B",	// 51 194
		"M W H M W M M H M M H W H M M H H M W M M H H B W H M M H M B M H M B B W W H W W H M B",	// 52 206
		"M W H M W M M H M W M M H M M B H M H W M M H B W B H W H M M H H M H B W W H W W H M B",	// 53 194
		"M W H M H B B M W H H B W B H W H M H H W H B M H H M H B H M H B H M H B H M M B B M B",	// 54 191
		"M W H M H W W B B H W H M M B B B H M H B H M H B W W B M M H M B M H M M B M M H B H B",	// 55 204 ** both good
		"M W H M H W W M H M M B M W B W B B W B H B B H M M H B H M H B H M W M H M B M H M M H",	// 56 207
		"M W H M H W W H W B M M H M B M H M M B M H B H M H B H M H B B H B W B H W H M M B M B",	// 57 204
		"M W H M H W W H W B M M H H M M H B W H M B H M H B H M H B H M M H M M H W H M M B M B",	// 58 215 ***
		"M W H M H W W H W B H H B W B M M H M M B M B B H M W M H M M B H M B B M W H M M B M B",	// 59 199
		"M W H M H W W H W B H H B W H H M H H M H B H M H B H M H B H M M H M M H W H M M B M B",	// 60 197
		"M W H M H W W H W W H B M H M M B M H M W M H M B M H M W M M H H B W B H W H M M B M B",	// 61 196
		"M W H M H W W H W W H M M B B M W W M H B H M W M H H M H B W B M W H H B W B H W H M W",	// 62 200
		"M W H M H W W H W W H M M W W M H B H M B M H M W M H M B M W H H B W B H W H M M B M B",	// 63 197
		"M W H M H H B B H M B B W W H W B B H W H M M W M M B M B B H M W M H M B M W H H B H B",	// 64 197
		"M W H M H H B M M B M H M M B M B M H M B B W W H W B B H W H M M H H M W M M H H B H B",	// 65 190
		"M W H M H H M W W M H B H M W M H H M H B W B M W H H B W B H W B W H W W H M M B M M W",	// 66 201
		"M W H M H H W M M W H W B W B M B H W B W W B H M M H B H M H B H M W M H M B M H M M H",	// 67 207 **
		"M W H W M W H H B W B H W H M M W M M B M B B H M W M W B B W W H W W H H B M M B H W B",	// 68 192
		"M H M B M H B H M H B H M H B W W H W W M H W M H B B M H M M H H B W B H W H M M B M B",	// 69 195
		"M H M M B B M B M W H M M B B H M W M W B B W B B W B H B H W W M H B H M M B B B H W B",	// 70 193
		"M H M M B B H M B B H M W M M H H B W H M B H M B B W W H W W M H W M M H W H M M B M B",	// 71 199
		"M H M M B H M H B H W M M W M M H H M M H B W H M B H M H B H M H B M B H W H H M B M B",	// 72 201
		"M H M M B H M H B H W M M W H H B W H H M H H M H B H M H B H M H B M B H W H H M B M B",	// 73 187
		"M H M M B H M H W M W H M M B B H M W M H M M B H M B B W B B W B H B H W M H M M B M B",	// 74 190
		"M H H M W M M H H M M H B W H M B H M H B H M H B H M B H M H B H W H B H W H H M B M B",	// 75 194
		"M H H M W H M M H B H B M B B M H M W H B H M H B W B B W B H B H W W M H B H M M B M B",	// 76 184
		"M H H M W H H B W H H M H H M H B H M H B H M H B H M B H M H B H W H B H W H H M B M B"		// 77 187
	)

	val callings41a = List(
		"B B B H M W B W H H B W B M H H B B H W B H H M M B H W W B W W H W W M H B H M W",	// 197
		"M W H B H M M B H H W W H W W B H B W B M M W M M H M M B B M B M B B H M H W W B",	// 207
		"M W H B H M M B H H W W H W W H B M M H B W B M M W M M H M M B B W B H M H W W B",	// 214 **
		"M H M B M B B H W H B W B M M W M M B H B W W H W W M H B H M M B H B M M B H W B",	// 199
		"M H W B H M H B W W H W W M H B H M M B H W B M M H B W B M M W M M H M M B H W B"		// 209
	)

	val callings41b = List(
		"B B B H M W B M M H H B B M W B W W H W B M H H M H H M M B H B M B H M H B H M W",	// 203
		"B B B H M H B W W H W B M H H M H H M H B M M H H M M B H M H B H M M B B B H W B",	// 209	1
		"B B M H B M B H M H B H M H B H M M H H M H H W H M H B W W H B B M M H H B B M B",	// 201
		"B B M H W W B W W H W B B M H W H B H M M B H H B M M H H M H H W H M W B H M M H",	// 218	3
		"B B W H H M H B W H H W H M H B W W H W B B M H H M M B H M H B H M M B B B H W B",	// 195
		"B M H W H M H B W W H W B H M M H H M M B M M H H W M H B M B H M H B H M M B M B",	// 205
		"B H M H H M H H W H M H B W W H W B H H B M M H H W M H B M B H M H B H M M B M B",	// 207
		"M H W M H B B M H M M H M M H H W H M H B W W H W B B M M H B H M H B H M M B M B",	// 214	7
		"M H W W M M H W H M H B W W H W B B M M W M M H M M B H B M B H M H B H M M B M B"		// 203
	)

	val callings40a = List(
		"B B B M B B W H B H B H M H W H B W H M M B B B M W M H W B B W H M H B H W W B",	// 188
		"B B B M B B W H M M W W H B H M H W H B W H M M B B H H W B B W H M H B H W W B",	// 199
		"B B B M B B W H M M W W H H B B W H M H W H B W H M B H W B B W H M H B H W W B",	// 192
		"B B B M B B W H M M W W H H B M W H B W H M B H H W H M B B B W H M H B H W W B",	// 191
		"B H M W W H B M B H H W W M H W H B W W B B W H H B W H M H B H W B M M W B B B",	// 186
		"B H M W W H B M B H H W W M H W H B W H M M W B M B B W H H B W H M H B H W W B",	// 197
		"M B M B B M W M H W W M H W H B W W B B W H H M B H W B M M H M W H M M B B B B",	// 189
		"M B M B B M W M H W W M H W H B W H M M H M W H M M B B M B B W H H M B H W W B",	// 192
		"M B M M W H B W B M M B B M B B W H M M W B B W W H H W M M H M W M M B H W W B",	// 186
		"M B M M W H B W B M M H M W B B W W H H W M M W B M B B W H B M W M M B H W W B"		// 196
	)

	val calling = callings44(24)
	//val calling = callings("new4")
	//val calling = callings("Seed 193.1")
	//val calling = callings("Bristol 5120 no.4")
	//val calling = "B M HMW M HB B MMB B B B B HB HB B HHW B B BW B M HB M HW BW HW HH"

	val methods = List(yorkshire, cambridge, superlative, pudsey, lincolnshire, lessness, london, bristol)
	//val methods = List(yorkshire, cambridge, superlative, pudsey, cornwall, lessness, london, bristol)

	//override val seed = Some("standard8/best.txt")

	//override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	def generate() = tunnel(this)
	//def generate() = prettyPrint()
	//def generate() = assessMethods()
	//def generate() = compareCallings()
	//def generate() = new TargetedSearch(this).varyFromSeed()

	//def generate() = new HelperCompositionSearch(this, helperPlan, "standard8/seed193.1.txt").varyFromSeed()
	//val helperPlan = new CompositionPlan(callings("Seed 193.1"), calls, getCallingMethod, musicDefs)

	//override def acceptNode(node: Node, leads: List[Lead]) = node.methods.forall(_==bristol) || node.music(0)>0


	def scoreFn(comp: Composition) = atwScore(comp)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*40 +
				ScoreFactory.balanceScore(comp)*2 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*5 + (if (comp.longestNoComRun<=3) 20 else 0) +
				(if (comp.isAtw) 100 + comp.music(0)+2*comp.music(1) else 0)
				//2000/(comp.falseScore+1) - 10*comp.falseScore

	def balanceScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*2 +
				ScoreFactory.balanceScore(comp)*40 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*3 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+comp.music(1)

	def noComScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*20 +
				ScoreFactory.balanceScore(comp)*5 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*30 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+comp.music(1)

	override lazy val musicDefs =
	{
		Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens())
	}

	override lazy val calls = List(Major.Bob, Major.Single)

	def compareCallings()
	{
		def callingToCOs(name: String) =
		{
			val comp = callings(name)
			val plan = new CompositionPlan(comp, calls, getCallingMethod, musicDefs)
			plan.calling.map{_.startLH.coursingOrder(getCallingMethod.plainPerm)}.toSet
		}

		val SEP = ";"
		val best = "Seed 193.1"
		val bestComp = callingToCOs(best)
		for (name <- callings.keySet)
		{
			val COs = callingToCOs(name)
			val sharedCOs = COs.intersect(bestComp)
			val extraCOs = COs--bestComp
			val missingCOs = bestComp--COs
			println(name + SEP +
					sharedCOs.size + SEP + missingCOs.size + SEP + extraCOs.size + SEP +
					missingCOs.toList.sorted.mkString(",") + SEP + extraCOs.toList.sorted.mkString(","))
		}
	}

}