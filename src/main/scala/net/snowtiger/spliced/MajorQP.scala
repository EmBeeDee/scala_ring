package net.snowtiger.spliced

import net.snowtiger.spliced.composition.{Composition, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object MajorQP extends SplicedGenerator with SearchDefinitionBase with StandardMethods
{
	//val calling = "WH BMH WM WHH W"
	//override val seed = Some("T 1248 4-Spliced (Score=4000936, COM=30, LR=4, ATW=167, music=79/16/23/8/2, LWM=11)  CSSS-B B- BSCLC-BSC-LC- BS-CSC L-BBBBL-B SCLCS- B- CL-SSC B(11/32) C(11/16) L(6/11) S(11/20)  (LTLM=9, LA=9, homes=8)")
	// T 1280 4-Spliced (Score=4002000, COM=27, LR=4, ATW=180, music=72/16/21/8/2, LWM=15)  CSSS-B B- BSCLC-BBL L-LS- BC-SSC L-BBBBL-B SCLCS- B- LC-SSC B(12/32) C(9/10) L(8/12) S(11/18)  (LTLM=9, LA=9, homes=9)
	// T 1280 4-Spliced (Score=4000732, COM=28, LR=4, ATW=170, music=77/16/23/8/2, LWM=12)  CSSS-B B- BSCLC-BLL L-LS- BS-CSC L-BBBBL-B SCLCS- B- CL-SSC B(11/32) C(9/12) L(9/14) S(11/19)  (LTLM=9, LA=9, homes=9)

	//val calling = "H WHH WW B B BMWH"
	// T 1376 4-Spliced (Score=4021550, COM=17, LR=7, ATW=196, music=58/18/21/0/2, LWM=15)  SSSSSSS- CCCC-L- LCL- BLLLLL-L LS-CCL-BS-BBB BBBB-LCC-C-CSC- B(9/18) C(12/12) L(12/10) S(10/18)  (LTLM=15, LA=17, homes=6) ATW
	// T 1248 4-Spliced (Score=4000944, COM=21, LR=5, ATW=158, music=78/26/25/0/4, LWM=10)  LSSSC- CSCC-B B- LSL- SSSC-B-B BBBLLC-BS-BBB BBL-SLC-C-L- B(13/38) C(8/10) L(8/6) S(10/24)  (LTLM=9, LA=11, homes=7)
	// T 1280 4-Spliced (Score=4000941, COM=24, LR=4, ATW=154, music=77/22/24/5/0, LWM=11)  SLSSC- CL-SSC- B- BBBLLL-B-B SC-BBB BLL-BS-SC CSC-LS CL-B B- B(13/40) C(8/12) L(9/10) S(10/15)  (LTLM=10, LA=11, homes=9)

	//val calling = "H WHH WWBHH"
	// T 1280 4-Spliced (Score=4000958, COM=32, LR=2, ATW=121, music=71/21/17/13/3, LWM=12)  LSL- SCCS-L- B- BS-L LC-B SC-CS- B- B(8/25) C(10/15) L(10/10) S(12/21)  (LTLM=8, LA=8, homes=7)
	// T 1280 4-Spliced (Score=4000751, COM=27, LR=4, ATW=153, music=83/18/24/7/0, LWM=9)  SLSSC- SL-B B- CSLCC- SL-B-B SC-BBB BLLC-SSSC- BS-CLCC-C-B B- B(11/38) C(11/20) L(7/9) S(11/16)  (LTLM=7, LA=9, homes=9)

	//val calling = "WH BM'H' W M W' /"
	// T 1248 4-Spliced (Score=4000871, COM=25, LR=5, ATW=155, music=67/12/21/7/0, LWM=14)  BBBBBL-B CSLCS- BCCLC-CS CSC'LS' BBBBBL-SLSC-LLLLL' B(12/36) C(9/9) L(11/15) S(7/7)  (LTLM=8, LA=12, homes=5)
	// T 1280 4-Spliced (Score=4000485, COM=21, LR=6, ATW=160, music=75/20/22/7/0, LWM=18)  BBBBBL-B CSLCS- LCCLS-BBB BLLC'CCSS' BS-SCLS-BBBBBB' B(17/50) C(8/10) L(7/8) S(8/7)  (LTLM=8, LA=11, homes=5)

	//val calling = "H WHH WW B MH WM WH"
	// T 1248 4-Spliced (Score=4000952, COM=27, LR=4, ATW=149, music=83/21/26/7/0, LWM=9)  SLSSC- BS-SSC- CLL- SL-B-B CS-BBB BLLC-BC- CSCS-CSLS-C-B B- B(10/36) C(10/21) L(7/6) S(12/20)  (LTLM=5, LA=11, homes=8)
	// T 1280 4-Spliced (Score=4000753, COM=30, LR=2, ATW=147, music=76/18/21/2/2, LWM=9)  SLSSC- SL-B B- CCSSL- CSSC-B-B SC-CLC-LC- LS-B BLLC-LC SL-B B- B(8/26) C(11/20) L(10/11) S(11/19)  (LTLM=7, LA=10, homes=10)

	val calling = "WH B M'H'H BMH BH WWW' /"
	override val seed = Some("T 1280 4-Spliced (Score=4000992, COM=36, LR=2, ATW=155, music=86/22/18/8/2, LWM=11)  BS-L- BLL-BSC'BS' CLSCS- BCSLC-BCS-BS- LSCLS-BSCLC- CL-B-B' B(10/46) C(10/11) L(9/8) S(11/21)  (LTLM=8, LA=8, homes=6)")

	//val calling = "HH'H M'W WHH M'H'H' M'W MH' W'WH MMMW'HH'H"
	// T 1280 4-Spliced (Score=4000777, COM=34, LR=2, ATW=143, music=77/25/17/7/2, LWM=13)  LSL- B' B- CSS'C-B-L- B- L'LS' B' SCS'C-B L-BC' LSB'CSLCS-L- L-B-B-C'L- B' B- B(12/44) C(8/9) L(11/7) S(9/17)  (LTLM=5, LA=9, homes=13)

	//val calling = "HH'H M'WWHH WMH' W'WH W'HH'H"
	//override val seed = Some("F 1280 4-Spliced (Score=4000798, COM=32, LR=2, ATW=148, music=78/29/19/2/2, LWM=10)  LSSCC- B' B- L'S-B-L- CCSLC- BS-CSLC-BC' LSB'CCS SL-L- BC'SCS- B' B- B(9/34) C(12/18) L(8/7) S(11/19)  (LTLM=5, LA=8, homes=11)")
	// T 1280 4-Spliced (Score=4000760, COM=29, LR=2, ATW=147, music=75/24/17/2/2, LWM=7)  LSSCC- B' B- L'C-B-L- CCSLC- CSCS-B BLLC-BC' LS'CCS SL-L- BC'L- B' B- B(9/28) C(13/23) L(10/9) S(8/15)  (LTLM=5, LA=11, homes=12)

	//val calling = "HH'H M'WWHH WMH' W'WH W'H'"
	// T 1280 4-Spliced (Score=4000743, COM=32, LR=2, ATW=145, music=71/22/14/4/0, LWM=8)  SSLSC- B' LSCSC- CSC'C-B-L- B- BS-B L-LC' LS'CCS SL-B SCCLS- BC'L' B(7/20) C(12/21) L(9/10) S(12/20)  (LTLM=5, LA=11, homes=11)
	// T 1280 4-Spliced (Score=4000741, COM=32, LR=3, ATW=150, music=71/21/17/4/2, LWM=9)  LSSSC- B' B- L'S-B-L- CCCLC- BS-CSLC-BC' LS'CCS SL-B LLS- BC'SCS' B(7/20) C(12/19) L(9/8) S(12/24)  (LTLM=5, LA=9, homes=10)

	// Goldthorpe
	//val calling = "HH WW MWH WW HH' /"
	//override val seed = Some("1376 4-Spliced (Score=4021492, COM=23, LR=7, ATW=196, music=31/18/7/0/0, LWM=25)  LLC- SCCLS- BBBLLL-L LS-CSLS-C-L- BBBBBBB-SCS SSSS-L- CLCCC' B(10/6) C(10/9) L(12/11) S(11/5)  (LTLM=8, LA=15, homes=7) ATW")
	//override val seed = Some("1248 4-Spliced (Score=4021492, COM=13, LR=9, ATW=196, music=43/19/16/0/3, LWM=21)  SSSSCSS- CCCCSCC- BBBBBBB-B-B SSC-C-L- LC-L LLLLLL-L- B' B(10/14) C(10/13) L(10/1) S(9/15)  (LTLM=27, LA=27, homes=7) ATW")
	//override val seed = Some("1312 4-Spliced (Score=4021545, COM=18, LR=8, ATW=196, music=45/17/15/2/3, LWM=21)  SSSSCSS- LCSCC- BBBBBBB-L CCCS-L SSC-C-L- LC-B-L- LLLLLLL' B(8/12) C(10/10) L(13/8) S(10/15)  (LTLM=12, LA=15, homes=7) ATW")

	val methods = List(london, cambridge, superlative, bristol)

	//val methods = List(yorkshire, cornwall, bristol, lessness)
	//val methods = List(superlative, bristol, yorkshire)

	//override val seed = Some("1312 4-Spliced (Score=4001795, COM=35, LR=2, ATW=175, music=109/48/24/8/1, LWM=16)  EYO- OBOBOY-E- YEO- BEOOB-YYEYY-B OE-BY-EO BBE-YEY-OBOB-E- B(10/33) E(10/35) O(11/28) Y(10/13)  (LTLM=4, LA=9, homes=6)")
	//override val seed = Some("1376 4-Spliced (Score=4021550, COM=17, LR=7, ATW=196, music=58/18/21/0/2, LWM=15)  SSSSSSS- CCCC-L- LCL- BLLLLL-L LS-CCL-BS-BBB BBBB-LCC-C-CSC- B(9/18) C(12/12) L(12/10) S(10/18)  (LTLM=15, LA=17, homes=6) ATW")

	//override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	/*
	for (m <- methods)
	{
		println(m.toString+":")
		AnalyseMethods.printBestCourses(m)
		println
	}
	*/

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	def scoreFn(comp: Composition) = ScoreFactory(comp).qpFinder

	override lazy val musicDefs =
	{
		Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens())
	}

	override lazy val calls = List(Major.Bob, Major.Single)

}