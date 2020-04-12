package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * George Salter spliced based on Alan Reading comp.
 * @author mark
 */
object SalterRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val sgurr = NamedMethod("Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val kenninghall = NamedMethod("Kenninghall", 10, "-56-14-56-30.14-14.50.14-14.50.14-14.50", "10")		// l
	val remus = NamedMethod("Remus", 10, "-3-4-2.5.6-34-5-6-67-6-7", "10")														// l

	val methods = List(remus, kenninghall, sgurr, triton, bristol)

	val calling = "H V'I'X X X'H'M' X'H X'H'HM S'F'M MM'W'H' M'W SS'M'"

	//override val seed = Some("AlanReadingRoyal/BIMY2.txt")
	//override val seed: Option[String] = Some("T 5000 5-Spliced (Score=5007748, COM=105, LR=2, ATW=405, music=658/179/55/26/10, LWM=21)  BK-KRRTTKRS'B' B-KSBBSK-BBSSS'B'SKBKTSB' R'TRKBKK- KTTTKTS'B'TKRKBKT-SRKSKRS- KRRKSS'RRS'B-SBKTTTK-SBKTRR'TSTR'SSBBS KB'T'RTTRT-SSSRRSB B-TRTTRTR'TRKB'TTSBBS B(24/117) K(24/152) R(24/103) S(26/137) T(27/149)  (LTLM=4, LA=21, homes=14) ATW")

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
				comp.falseScore*30 -
				comp.longestNoComRun*18 + comp.com +
				comp.music(0)*2+comp.music(1)+comp.music(2)+comp.music(3)*2+comp.music(4)


	override lazy val musicDefs = Array(new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music56Rollup(), new Music65Rollup())

	override lazy val calls = List(Royal.Bob, Royal.Single)

	val shortNodes = Map(Row("1423569078")->3, Row("1957820463")->1, Row("1365482079")->1, Row("1573960482")->2)

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		if (inSeed)
			true
		else
			node.size <= shortNodes.getOrElse(node.startLH, 11)
	}

}