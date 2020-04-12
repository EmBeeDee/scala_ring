package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, Music, NamedMethod, PN}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

import scala.io.Source

/**
 * @author mark
 */

object Davies10Holdsworth extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	val strakers = NamedMethod("Z Straker's Passage", 8, "36x56.14.58x12.38.14x14.58.14x14.58", "12")	// a
	val tactical = NamedMethod("t Tactical", 8, "36x56.14.58x12.38x14x58x14x58 ", "12")               // a
	val xenolite = NamedMethod("Xenolite", 8, "x58x14.58x12.38.12x12.38x14x78", "12")                 // d
	val coopers = NamedMethod("c Coopers", 8, "38x56.14x56x1236x14x58x14x78", "12")										// a
	val erica = NamedMethod("Erica", 8, "38x56.14.56x58.16x14x1458x14x18", "12")											// b
	// Bouchavenes

	// "The" 5056
	val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MW MW MWW MH"
	// The 5120 version
	//val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MMBWW MW MWW MH"

	//val calling = "BWH WW B B B MHH BHH B B" // Alpha
	//val calling = "B H H W B B B B H H W B M W W"	// Beta
	//val calling = "M M H B B B B M H H B B M H W" // Gamma
	//val calling = "M M H WW B B B M H H B B M W W" // Gamma bis
	//val calling = "B W W B M B H W B B B B H B W" //Delta

	//val methods = List(adelaide, superlative, malpas, cornwall, deva, lessness, bristol)
	//val methods = List(henley, yorkshire, adelaide, superlative, malpas, cornwall, deva, lessness, bristol)
	//val methods = List(yorkshire, adelaide, superlative, malpas, cornwall, deva, lessness, bristol)
	//val methods = List(rook, queenCamel, adelaide, yorkshire, malpas, superlative, cornwall, deva, lessness, bristol)
	//val methods = List(coopers, xenolite, adelaide, yorkshire, malpas, superlative, cornwall, deva, lessness, bristol)
	//val methods = List(lessness, cornwall, deva, bristol)

	val methods = List(rook, queenCamel, bouchavesnes, lowerBeeding, henley, tavus, hanwell, cornwall, bristol)
	//queenCamel, bouchavesnes, rook, lowerBeeding, henley, tavus, hanwell

	//override val seed = Some("onepart2\\BCDL.txt")
	//val seed = Some("JamesHoldsworth\\pretty.txt")
	//val seed = Some("5024 7-Spliced (Score = 7001681, COM = 119, music = 252/70)  B- MLCBCS-B BCCDL-LLAA- ALAAAS-BL-CCBL-CCMD CL-LS-CB BMCDL-BM B- BBADB-LSD M-MCM MMCL- BCMDL-MAMC B- BLLLMB-MMCDC-SLS-LASA-BDM- C-BDDA DBM-SSS M-LLCL-L SSS-DDDB-DBDAM L-BBA AMB-SDASD M-DDDB-CCCL-MAM ASLS-MCC CDC- B(26) C(26) D(22) L(25) M(24) S(17) A(17)  (longrun=3, homes=22) ATW")

	def scoreFn(comp: Composition) = score1(comp)

	//def generate() = tunnel(this)
	def generate() = new MultiMethodGenerator(this, extraMethods).multi(200000)
	//override def generate() = prettyPrint()

	def score1(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				ScoreFactory.strictLenScore(comp) +
				2*(ScoreFactory.balanceScore(comp)) +
				20*(comp.atwScore) + (if (comp.isAtw) 50 else 0) +
				(1*comp.music(0) + 4*comp.music(1) + 10*comp.music(3) + 2*comp.music(5)+ 30*comp.music(6)) -
				10*comp.longestNoComRun -
				comp.leadsWithoutMusic

	override lazy val musicDefs: Array[Music] =
		Array(mainMusic, new Music56Rollup(), new Music65Rollup(), new MusicQueensRow(), new MusicLB(4), new MusicRun(6), new MusicRun(8))

	val mainMusic =	new CompositeMusic(new MusicRun(4), new Music65Rollup(), new MusicQueens())

	val methodAssessor = MethodAssessor(8)

	val badStarts = Set(PN.parse("56x56", 8), PN.parse("34x34", 8))

	val extraMethods = methodAssessor.parseMethods(Source.fromFile("surprise.csv")).
			filter(methodAssessor.isVeryGood).
			filter((m)=> !badStarts.exists{m.lead.startsWith(_)}).
			filter(methodAssessor.hasNewWork(methods))
			//filter(methodAssessor.hasNewStart(methods))
	//filter(methodAssessor.hasNewLHGroup(methods))
}
