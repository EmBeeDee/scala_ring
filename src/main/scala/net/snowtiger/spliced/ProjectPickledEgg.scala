package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, PN, Row}
import net.snowtiger.spliced.composition.{Composition, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

import scala.io.Source

/**
 * @author mark
 */

object ProjectPickledEgg extends SplicedGenerator with SearchDefinitionBase with StandardMethods
{
	val methodAssessor = MethodAssessor(8)
	val londonReplacements = methodAssessor.parseMethods(Source.fromFile("SurpriseMajor.txt")).
			filter(_.lead.startsWith(List(PN("38"),PN("x"),PN("38")))).
			filter(methodAssessor.isGood).
			filter(_.wrongPlaceFactor>0.5)
	londonReplacements.foreach((m)=> println(m.namePlusClass+" "+m.difficulty+" "+m.outputPN()))

	val stMarysGate = NamedMethod("G St Mary's Gate", 8, "38-38.14.56-58.16.34-14.58-16-18", "12")				// f
	// St Mary's Gate S 48 38-38.14.56-58.16.34-14.58-16-18, 12
	// T 5088 7-Spliced (Score=7019915, COM=123, LR=6, ATW=313, music=129/33/21/6/0/0, LWM=105)  EEOEO-SYS- CSS-OGGE-G OE-SC- GGEEOE- E-YSYC SYC-BBBBBB-O- YCSCYCY- CCS-C-G B(18/6) C(30/15) E(27/45) G(18/16) O(18/2) S(27/34) Y(21/11)  (LTLM=34, LA=34, homes=8)

	val isambard = NamedMethod("Isambard", 8, "38-38.14-12-38.14-12.58.14-16.78", "12")
	// Isambard S 48 38-38.14-12-38.14-12.58.14-16.78, 12
	// T 5088 7-Spliced (Score=7019542, COM=108, LR=4, ATW=307, music=121/33/30/0/0/0, LWM=100)  I-E- E-I OOBBB-E CII-SS- CYSCCSY- YSC-EEEEO E-OOO-SYC- CYYYSCY- SEOB-S-YIIC B(12/0) C(27/19) E(27/20) I(18/21) O(21/8) S(27/24) Y(27/29)  (LTLM=16, LA=39, homes=9)

	val otford = NamedMethod("T Otford", 8, "38-38.14-12-38.14-14.58.14-36.78", "12")
	// Otford S 47 38-38.14-12-38.14-14.58.14-36.78, 12
	// T 5088 7-Spliced (Score=7019664, COM=108, LR=6, ATW=309, music=130/42/25/6/0/0, LWM=109)  EOOE-CYY- O-S-SSO-OO- ECYEYTC- SCY-EC SSC-BBBBBB-CTTS- YTYESYO- CCS-OOO-E B(18/6) C(27/16) E(21/34) O(30/34) S(27/19) T(12/5) Y(24/16)  (LTLM=29, LA=32, homes=7)

	val calling1 = "MMH BBBBMHH BBMH W"
	// T 5184 6-Spliced (Score=6018110, COM=114, LR=4, ATW=282, music=182/48/34/18/0/0, LWM=102)  O-EEC-BBOBB B- OBEEY-BY-CO-SO-EO E-OBOO- CYYYCYC- ECSSO-SY YC-CS SSS-EC- CE-E B(24/12) C(30/20) E(30/64) O(30/40) S(24/33) Y(24/13)  (LTLM=19, LA=29, homes=9)
	// T 5088 6-Spliced (Score=6014731, COM=120, LR=3, ATW=276, music=195/60/38/24/0/0, LWM=95)  E-OY-YCYC- ECSEY-SE-YC CS-SO-BEE O-EY- SCCSYSS- YS-BC-CES-BBOBB B- CO-OOBBO B(27/30) C(30/19) E(24/50) O(24/56) S(30/27) Y(24/13)  (LTLM=20, LA=20, homes=8)

	val calling2 = "B WWB MBH WBBBBH B W"
	// T 5088 6-Spliced (Score=6013784, COM=120, LR=5, ATW=277, music=191/54/39/24/0/0, LWM=103)  EOB-BBOC-YEYCY-SSY YC-BCS-SCYS CY-SS- SO-B CY-BS-CO-BEE EEE-OB B- OBOC-EO-E B(30/36) C(27/16) E(27/67) O(24/40) S(27/21) Y(24/11)  (LTLM=12, LA=30, homes=8)
	// T 5088 6-Spliced (Score=6014648, COM=105, LR=4, ATW=273, music=190/60/40/9/0/0, LWM=96)  BOOC-CY SE-YEO-SEC-BSS-BE-SC- OBOE-SSS CY-YY YC-CO-OOOB-ESYYE- EEE-BBOC-B B(24/35) C(24/18) E(30/30) O(30/66) S(27/32) Y(24/9)  (LTLM=7, LA=21, homes=6)

	val calling3 = "BHH WBBBBHH WBM WW"
	// T 5088 6-Spliced (Score=6018022, COM=111, LR=5, ATW=279, music=192/60/44/24/0/0, LWM=103)  EOB-SY- OC- CYCS-SSS CY-YY YC-EO BEO-BEE EEE-OE- OS- BBBOBB-SCS CY-BSS-Y-OC-E B(27/32) C(24/11) E(30/66) O(24/44) S(30/26) Y(24/13)  (LTLM=6, LA=27, homes=10)
	// T 5088 6-Spliced (Score=6014811, COM=126, LR=3, ATW=275, music=208/66/39/24/0/0, LWM=95)  EOOY-SS- OC- BY-SYY YC-CO-SE-BBE EOB-ESYCO- EEY- CO-B OBOY-BYS-BS SE-CEYCC-E B(24/16) C(24/15) E(30/102) O(27/28) S(24/30) Y(30/17)  (LTLM=8, LA=18, homes=9)
	// T 5088 6-Spliced (Score=6014790, COM=123, LR=2, ATW=273, music=212/66/39/24/0/0, LWM=93)  EOOY-SS- OC- BY-SSY YC-CO-SE-BBE EOB-ESYCO- EEY- CO-B OBOY-BSS-BS SE-CEYCC-E B(24/16) C(24/15) E(30/102) O(27/28) S(30/38) Y(24/13)  (LTLM=8, LA=18, homes=9)
	// T 5088 6-Spliced (Score=6010433, COM=108, LR=4, ATW=271, music=215/66/40/24/0/0, LWM=94)  EOOY-SS- OC- CYCY-SSY YC-CO-SE-BBE EOB-OO- OY- BBBBEO-B CY-BSS-ES SE-CEYCC-E B(27/18) C(27/19) E(27/86) O(27/42) S(27/35) Y(24/15)  (LTLM=21, LA=22, homes=9)
	// T 5088 6-Spliced (Score=6010428, COM=87, LR=4, ATW=273, music=216/66/43/24/0/0, LWM=90)  EOOY-SS- EEC- YYYS-SSC CC-CY CC-SO-EEE EOB-OO- OY- BBBOBB-B CY-BSS-Y-YEYCC-E B(24/16) C(30/20) E(27/74) O(24/52) S(24/33) Y(30/21)  (LTLM=29, LA=29, homes=9)


	val calling4 = "BBBWH BB MBH B"
	// T 5088 6-Spliced (Score=6009839, COM=99, LR=5, ATW=264, music=191/60/31/24/0/0, LWM=100)  OBB-SO-YC SS-YS YSSY-CYY- CEOBBB-BBE EEE-OOOOO E-CCSO-BYCCO- BYSCE-EEE B(27/18) C(24/12) E(30/73) O(30/38) S(24/39) Y(24/11)  (LTLM=19, LA=25, homes=7)

	val calling5 = "WH MWBHH MMWHH MW"
	// T 5088 6-Spliced (Score=6009787, COM=126, LR=3, ATW=271, music=165/57/31/9/0/0, LWM=95)  OEOE-E- O-Y-SYC YC-SEOBE- YSSCSCC- CYY-SCCS SSC-BOOB-YEO B- EOBBBO- BOS-Y-E B(24/9) C(27/15) E(24/34) O(30/44) S(30/45) Y(24/18)  (LTLM=15, LA=23, homes=9)

	//val dfm = "MWHH BHH WBBH MMH  MWHH BHH BBH MMH"// T 5088 6-Spliced (Score=6010219, COM=114, LR=7, ATW=259, music=192/72/32/24/1/0, LWM=102)  O-S-O- B- SY-SS- B- YE-SCO-YE-SC- CCY-ES E-YO- E-C-YYC- B- BBBB-BBE- YEE- OE-CO-OO- O-OC-SE- B(27/6) C(24/15) E(30/72) O(30/50) S(24/32) Y(24/17)  (LTLM=12, LA=29, homes=13)
	//val dfm = "MWHH BHH WBBH MMH  BMH MMH"	// T 5088 6-Spliced (Score=6009909, COM=135, LR=2, ATW=262, music=181/60/33/24/1/0, LWM=98)  E-OOE-SCS- B- ECCYO-BBE- YSYYE- SO-SSO-EC-SC- BOC-SCEYY-CE- OBB-OB E-EY- E-OC-SO- B(21/8) C(27/12) E(33/35) O(30/62) S(27/44) Y(21/20)  (LTLM=11, LA=18, homes=9) [10442 moves/s]
	val dfm = "BMH MMH MWHH BHH WBBH MMH "	// T 5088 6-Spliced (Score=6010313, COM=120, LR=4, ATW=266, music=207/66/45/24/0/0, LWM=93)  OBOS-BBB E-EC- E-OC-SO- E-Y-CCS- SSESC- OO-BBE- YYYYE- CO-SSO-SE-OO- BCYECY-B-YE- B(24/10) C(24/12) E(30/59) O(30/82) S(27/25) Y(24/19)  (LTLM=16, LA=21, homes=9)
	val mbd = "WMW BBH MMH  WM BBH MMH"

	val calling = mbd

	//override val seed: Option[String] = Some("ppe/core6_3parts.txt")

	//val methods = List(cambridge, yorkshire, cornwall, superlative, bristol, lessness, london)
	val methods = List(cambridge, yorkshire, cornwall, superlative, bristol, lessness)
	//override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	//override def acceptNode(node: Node, leads: List[Lead]) = node.methods.forall(_==bristol) || node.music(0)>0

	def scoreFn(comp: Composition) = balanceScore(comp)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*300 +
				comp.atwScore*60 +
				ScoreFactory.balanceScore(comp)*5 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*5 +
				(if (comp.isAtw) 50 else 0) +
				comp.music(0)+comp.music(1)*2

	def balanceScore(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*200 +
				comp.atwScore*10 +
				ScoreFactory.balanceScore(comp)*30 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*3 +
				(if (comp.isAtw) 50 else 0) +
				5*(comp.music(0)+comp.music(1)*2)

	def noComScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*25 +
				ScoreFactory.balanceScore(comp)*5 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*30 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+comp.music(1)

	override lazy val musicDefs =
	{
		Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueensRow(), new MusicWhittingtonsRow())
	}

	override lazy val calls = List(Major.Bob)

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		if (node.startLH == Row("15324786") || node.startLH == Row("17234586"))
		{
			//val inSeed = node.methodsUsed == Set(getCallingMethod)
			//println("Got row "+node.startLH+" "+(node.methods.last==bristol)+" "+node)
			node.methods.last==bristol
		}
		else
			super.acceptNode(node, leads)
	}
}