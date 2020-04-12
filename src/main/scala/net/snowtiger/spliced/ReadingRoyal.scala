package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * Alan Reading comp.
 * @author mark
 */

object ReadingRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val moray = NamedMethod("Moray Firth", 10, "-50-16-1270-18-14-30-16-70-58-90", "12")								// b
	val independence = NamedMethod("Independence Day", 10, "30-50.14-50-30-34-50.16-16.70-16-70", "12")	// f
	val cambridge = NamedMethod("Cambridge", 10, "x30x14x1250x36x1470x58x16x70x18x90", "12")						// b
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f


	//val methods = List(moray, independence, bristol, yorkshire)
	val methods = List(triton, cambridge, bristol, yorkshire)

	val calling = "WHH WW MMH W MH W WHH WW MMH W MH W"

	//override val seed = None
	override val seed = Some("T 5000 4-Spliced (Score=4006221, COM=89, LR=8, ATW=324, music=552/175/42/18/7, LWM=5)  YT-YTCBCY- BCBBCB- YT-CBBBYB-CTYBYY T-TTC-TY- YBBYCC-CTYYC-TTTTTTTT- YT-CCYTYB-T- YTCBYCTC- TC-YBBYBB-T T-B BCBCC-YBYCTCY- CBBBBBBB-YTYYC-CCBBYC- YBYYTCY-CBYYTC B(33/196) C(30/95) T(29/145) Y(33/116)  (LTLM=5, LA=24, homes=12) ATW")
	// T 5000 4-Spliced (Score=4006638, COM=112, LR=2, ATW=324, music=527/162/42/20/4, LWM=4)  YT-YBBYC- BCBBCB- YT-CBBYBB-T CYBBY-TC YTCBYC-TY- BBYBBCB-T YBYCTC-TC- YT-CCYTYB-CTCBYY- CTCYTYBY- TC-YBCYB-T T-B YBBCB-YT- CYTCBCB-YTCYC-YCTYCTYC- YBBYBBC-YBYCTC B(35/194) C(32/95) T(23/109) Y(35/129)  (LTLM=6, LA=15, homes=14) ATW
	// T 5000 4-Spliced (Score=4006236, COM=86, LR=8, ATW=324, music=549/168/46/23/4, LWM=3)  YT-YTCBYY- BCBBBBC- YCTCCTCY-CBBBYB-CTYBCY T-TTC-TY- YYTYYTYY-YBYC-B BCBBCB- CT-CYYTCC-CTCBYY- YYTYYC- TTTTTTTT-CBBBCB-T T-B YBBCC-TY- CYBBCB-BBBYC-YT- CT-CBYCTC B(32/197) C(32/102) T(29/140) Y(32/110)  (LTLM=5, LA=19, homes=13) ATW
	// T 5000 4-Spliced (Score=4006257, COM=86, LR=8, ATW=324, music=564/159/44/24/4, LWM=4)  YT-T- BCBBCC- YCTCYTCY-CBBBBBB-CTYBBC-TTC-TY- YYTYYTYY-CBYY-B BYBBCB- CT-CYYTCY-CTCBYY- CYBYC- TTTTTTTT-CBBBCB-T T-TTC-TY- CBBCCB-BBBYC-CYTYBYY- CBYYTCY-CBYCTC B(32/194) C(32/104) T(29/156) Y(32/110)  (LTLM=4, LA=22, homes=11) ATW
	// T 5000 4-Spliced (Score=4006626, COM=109, LR=2, ATW=324, music=537/146/35/19/4, LWM=7)  CT-T- BCBBCB- YT-CBBYBB-T CYBBC-TC YTYBYC-TY- BBYBBCB-T CYTYCTY-TC- YBCYTCY-CCYTYB-YTCYTCY- BCBBCC- YCTCYTCY-CCYTYB-T T-B YBBCB-CT- CYBBCB-YBYC-TY- CBCBCB-YYTYYTY B(33/197) C(34/96) T(24/111) Y(34/133)  (LTLM=9, LA=16, homes=14) ATW
	//override val seed = Some("AlanReadingRoyal/BIMY2.txt")

	//def generate() = prettyPrint()
	def generate() = tunnel(this)
	//def generate() = genetic(this)
	//def generate() = new MultiMethodGenerator(this, extraMethods).multi(400000)

	def scoreFn(comp: Composition) = score(comp)

	def score(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*15 +
				ScoreFactory.balanceScore2(comp)*4 +
				ScoreFactory.strictLenScore(comp, 0.3) -
				comp.falseScore*20 -
				comp.longestNoComRun*20 + comp.com*5 +
				comp.music(0)*2+comp.music(1)+comp.music(2)+comp.music(3)*2+comp.music(4)


	override lazy val musicDefs = Array(new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music56Rollup(), new Music65Rollup())

	override lazy val calls = List(Royal.Bob, Royal.Single, Royal.BigBob)
	//val compositeMusicDef = new CompositeMusic(new MusicRun(4), new MusicRun(5), new MusicRun(6), new MusicAscendingRunTenorBehind(4,2), new MusicAscendingRunTenorBehind(5,2), new Music4Course(), new Music56Rollup(), new Music65Rollup(), new MusicTittumsRow())
	//override lazy val musicDefs = Array(compositeMusicDef, new Music56Rollup(), new MusicTittumsRow(), new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music4Course())

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		if (inSeed)
			true
		else if (node.startLH==Row("1243759608"))
			containsLead(node, leads, "1536284079", bristol)
		else if (node.startLH==Row("1645739208"))
			containsLead(node, leads, "1352684079", bristol)
		else
			true
	}

	private def containsLead(node: Node, leads: List[Lead], lh: String, method: NamedMethod): Boolean =
		node.methods.zip(leads.map{_.startLH}).toSet.contains(Tuple2(method, Row(lh)))

}