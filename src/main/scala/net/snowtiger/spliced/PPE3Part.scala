package net.snowtiger.spliced

import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object PPE3Part extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	//val calling = "BWH WW B B B MHH BHH B B" // Alpha
	// T 5088 6-Spliced (Score=6002142, COM=114, LR=3, ATW=255, music=199/66/40/18, LWM=96)  CBB-BL-C- CCLL-YL@@Y-YSS S@-B@-@C-BC L-@YYY- B- LCB-@Y- @SYLS- CBCS-@S SS-BLL @(27/13) B(27/38) C(27/42) L(27/63) S(27/28) Y(24/15)  (LTLM=16, LA=20, homes=9)

	//val calling = "BWH V'V'WW B B B MHH BHH B B" // AlphaSplit

	//val calling = "B H H W B B B B H H W B M W W" // Beta
	// T 5088 6-Spliced (Score=6002161, COM=123, LR=4, ATW=266, music=200/63/45/15, LWM=93)  CBCS-SY- LLS- @Y@S-CLC-Y@ @Y-SL-BLCCB-LC C@- B- BBBCBB-SYY S@-@LS-Y-L @@YS-L @(27/19) B(27/18) C(27/56) L(27/62) S(27/27) Y(24/18)  (LTLM=9, LA=21, homes=9)
	// T 5088 6-Spliced (Score=6002952, COM=114, LR=3, ATW=257, music=215/66/42/24/0/0, LWM=94)  LCCY-SS- C@- BBBCL-SYY @@-@S YS-SC-BLL LCB-CC- LYL- BY-B YY-B@S-BS SC-@LY@@-L @(24/14) B(27/22) C(27/58) L(27/73) S(27/33) Y(27/15)  (LTLM=8, LA=23, homes=10) [2163 moves/s]
	// T 5088 6-Spliced (Score=6002967, COM=117, LR=3, ATW=253, music=226/66/45/24/0/0, LWM=89)  LCCY-SS- CY- BBBCL-SSY YY-B@-SC-BBL LCB-LSY@L- C@- @C-B @Y-LSS-BLCC-@LY@@-L @(24/11) B(27/22) C(30/48) L(30/95) S(24/36) Y(24/14)  (LTLM=19, LA=19, homes=8)

	val calling = "B H H W B B B B V'V' H H W B M W W" // BetaSplit
	// T 5088 6-Spliced (Score=6002658, COM=120, LR=2, ATW=254, music=240/90/44/24/0/0, LWM=89)  LCCS-BBC- CY- @L-B YY-BS-SC-BBL LCB-C'C LLCC'SY@L- C@- SY@Y-SS@ S@-@LS-Y-L @C-L @(24/17) B(21/22) C(36/72) L(30/96) S(27/24) Y(21/9)  (LTLM=9, LA=30, homes=10)
	// T 5088 6-Spliced (Score=6002649, COM=120, LR=2, ATW=245, music=246/90/45/24/0/0, LWM=88)  LCCS-BBC- CY- @L-CLL-BS-SC-BBL LCB-C'C LLCC'SY@L- C@- SY@Y-SS@ S@-YY C-Y-@LC-L @(21/15) B(18/20) C(42/80) L(33/100) S(24/22) Y(21/9)  (LTLM=9, LA=30, homes=9)
	// T 5088 6-Spliced (Score=6002652, COM=120, LR=2, ATW=247, music=244/90/45/24/0/0, LWM=88)  LCCS-BBC- CY- @L-CLL-BS-SC-BBL LCB-C'C LLCC'SY@L- C@- SYSY-SS@ S@-YY L-Y-L @C-L @(18/12) B(18/20) C(39/76) L(36/103) S(27/24) Y(21/9)  (LTLM=9, LA=30, homes=10)

	//val calling = "M M H B B B B M H H B B M H W" // Gamma
	// T 5088 6-Spliced (Score=6002176, COM=117, LR=5, ATW=264, music=212/78/39/18, LWM=93)  C-B-YL- BBBLL@-YC-BY-BBB CL-SS S@S-LS- Y@YS@S@- L@SSL-BY-YS L-YC- @Y@@-CCCC @(27/12) B(27/38) C(24/58) L(24/43) S(30/41) Y(27/20)  (LTLM=19, LA=27, homes=8)
	// T 5088 6-Spliced (Score=6002177, COM=114, LR=5, ATW=263, music=214/78/39/18, LWM=92)  C-B-YL- BBBLL@-YC-BY-BBB CL-SS S@S-LS- YSY@@S@- L@SSL-BY-YS L-YC- @Y@@-CCCC @(27/12) B(27/38) C(24/58) L(24/43) S(30/43) Y(27/20)  (LTLM=19, LA=27, homes=8)
	// T 5088 6-Spliced (Score=6003063, COM=120, LR=5, ATW=259, music=215/78/40/18/2/0, LWM=91)  C-B-YL- BBBLL@-SC-BY-BBB CL-SS @YS-LS- Y@Y@@S@- L@SSL-LY-CB L-YC- @Y@S-CCCC @(27/16) B(27/38) C(27/60) L(27/43) S(27/39) Y(24/19)  (LTLM=10, LA=25, homes=8)

	//val calling = "M M H V'V' B B B B M H H B B M H W" // GammaSplit
	// T 5088 6-Spliced (Score=6003276, COM=102, LR=5, ATW=258, music=225/96/38/18/2/0, LWM=93)  C-B-YL- BBBLL'LCBCC'@-YY @@-@S @Y-CB BLL-SS SSS-@Y@S- B- YY-LY-LYS-L@- BS-CCCC @(24/14) B(27/58) C(27/66) L(27/38) S(27/27) Y(27/22)  (LTLM=20, LA=26, homes=9)
	// T 5088 6-Spliced (Score=6003172, COM=117, LR=3, ATW=259, music=232/84/40/24/3/0, LWM=84)  L-LY @@Y-YL- LSSC'Y@YY@S @'S-CCCB-@L-SC-BBL C-YC- SLL- YS-B@-BSS-B@- @L-CCBBC @(27/37) B(24/34) C(30/80) L(27/34) S(27/34) Y(24/13)
	// T 5088 6-Spliced (Score=6002625, COM=111, LR=5, ATW=255, music=235/96/37/24/2/0, LWM=98)  C-L@ C-YL- BBBLL'LCBLLC'@-BY-YL-CB BLL-BC BB-SC- B- CC-SY S@-@S @SS-YC- BS-CCCC @(15/12) B(36/84) C(42/80) L(30/35) S(21/16) Y(15/8)  (LTLM=31, LA=31, homes=10)
	// T 5088 6-Spliced (Score=6002611, COM=129, LR=2, ATW=258, music=231/90/36/24/0/0, LWM=88)  L-CS-YY@S- LSSL'C LLCC'S-BY-@L-SC-BBL C-YC- B- BCB-S@ S@-@S @SS-Y@Y@- YL-CLLCC @(24/21) B(18/10) C(33/94) L(30/71) S(33/27) Y(21/8)  (LTLM=17, LA=22, homes=9)

	//val calling = "M M H WW B B B M H H B B M W W" // Gamma bis

	//val calling = "B W W B M B H W B B B B H B W" //Delta
	// T 5088 6-Spliced (Score=6001896, COM=117, LR=3, ATW=259, music=209/66/37/24/2/0, LWM=95)  LCCY-SS YC-B-L CBB-@LS-BS @@-@Y- BY-S@@ YY-YS @@-SC-BLL LCB-LSY@L- CBB-CCS-L @(27/15) B(27/20) C(27/42) L(27/84) S(27/34) Y(24/14)  (LTLM=13, LA=24, homes=9)
	// T 5088 6-Spliced (Score=6001879, COM=120, LR=3, ATW=260, music=218/66/41/24/0/0, LWM=94)  LCCY-SS LY-SYY @C-B @S-BSS-LS SY-@@- @L-CCB-SY Y@-SC-BBL LCB-CL- CBC@-BY@@-L @(27/13) B(24/22) C(30/64) L(24/76) S(30/31) Y(24/12)  (LTLM=13, LA=19, homes=9)

	//val calling = "B W W B M B H W B B B B V'V' H B W" //DeltaSplit
	// T 5088 6-Spliced (Score=6002648, COM=123, LR=3, ATW=254, music=244/90/44/24/2/0, LWM=92)  LCC@-BC-YLC-B @@-BSS-Y@YL-YS- SL-SSC-B@-YC-BLL LCB-L'C LLCC'SY@L- CBB-CCS-L @(18/12) B(24/28) C(39/66) L(36/111) S(24/19) Y(18/8)

	//val calling = "BV'M'FH W MWWH WW MH BV'M'FH W MWWH WW M" //DFM

	//val calling = "WH MMWH BH MMW' M'W MW"
	// T 5088 6-Spliced (Score=6001803, COM=120, LR=3, ATW=269, music=175/48/38/24/2/0, LWM=94)  @@@Y-C- @@S-CY-S-SLY@L@- SS-YS- BLLS-CLCL C-CBBL'YLC BCY'BBCBB-@SS L-@-YYY @(27/23) B(24/30) C(27/12) L(27/45) S(27/17) Y(27/48)  (LTLM=21, LA=29, homes=7)

	val dfm3part3 = "MWHH BHH WBBH MMH MWHH BHH BBH MMH"	// Don: T 5184 6-Spliced (Score=6009648, COM=132, LR=3, ATW=233, music=195/66/33/24/3, LWM=100)  O-Y-E- B- SY-OB B- B- CE-SCO-YE-OO- CCY-ES E-SO- O-Y-E- B- SY-OB B- B- CESCO-YE-OO- CCY-ES E-SO- B(24/14) C(24/17) E(30/18) O(36/108) S(24/27) Y(24/11)  (LTLM=54, LA=54, homes=16)

	//val methods = List(london, superlative, cambridge, yorkshire, lessness, cornwall, bristol)
	val methods = List(superlative, cambridge, lessness, cornwall, bristol, yorkshire)
	//override val seed = Some("PPE\\core7_gamma.txt")
	def scoreFn(comp: Composition) = ScoreFactory.musicFinder2(comp)

	override lazy val musicDefs = Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens(), new MusicQueensRow)

	def generate() = tunnel(this)

}