package net.snowtiger.spliced

import net.snowtiger.ringing.PN
import net.snowtiger.spliced.composition.{Call, Composition, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object Regan6 extends SplicedGenerator with SearchDefinitionBase with StandardMethods
{
	val calling1 = "BBB MWH' W'W BBH BH BB MMBBBBBHH' W'W MH BH WW B"
	val calling2 = "H W BH MBBBBBH B MH BH W B MMH MW MW MW MW MWW MH"
	val calling3 = "B M H B W H M B H M H B H M H B W W H W W M H W H H M M B B H H M M B M H W H M M B M B"
	val calling4 = "WHH BB MMWWH MHH MMBH WHH WWH WW"

	val calling5 = "MMWWHHH"*5    // Middleton

	val calling5a = "MMWWHHVVVH"*5    // Middleton
	// T 5024 6-Spliced (Score=6010744, COM=63, LR=7, ATW=293, music=99/18/21/5/5, LWM=107)  YSS-SYYY YYY-BBBBBB-E EY-E- B- S-YSSSSS S-B-ECCC- CSS-CCCC L-LLLLE-B-L- B- LLLEE-EE EEELL-SLL-LL- L-B-Y-CCC CCCC-E- EEC- C-CCCCCS S-EE Y-EYYY- YCC-B-ELLLL-B-SSS- B- Y-YYYYSS S-EE Y-YSSSSS- CCY-CE E-BBBBBB-B-CYY- SLL- S-EE EELLL-B-EL- B(22/7) C(28/24) E(28/6) L(26/21) S(28/18) Y(25/23)  (LTLM=41, LA=47, homes=27) [12992 moves/s]

	val calling5b = "MMWWHVVVHVVVH"*5    // Middleton
	// T 5088 6-Spliced (Score=6004964, COM=68, LR=10, ATW=288, music=94/23/16/6/11, LWM=114)  E-YE E-BBBBBB-B-E- C-LSSS S-B-CCYYYY- S-B-B-EE- CSS-SSSS SCC-BBBBBB-B-YYY- S-LL S-B-EE- Y-YLL-B-EE- E-B-S-SSC CCCC-L- S-YLL-B-EE- S-CCCCYY Y-YYYYYY Y-EE- E-CL L-S-B-L- C-CLL-LL C-CCCL- Y-B-B-EYYY- YCC-CE E-C-B-SSS- S-LL LLLLL-B-LL- Y-B-EE EEEEL-ESSS- B(27/13) C(26/16) E(26/17) L(26/8) S(27/30) Y(27/10)  (LTLM=14, LA=38, homes=27)
	// T 5056 6-Spliced (Score=6004493, COM=79, LR=7, ATW=283, music=112/27/23/5/13, LWM=102)  SSS-EEY-BBBBBB-B-YSS- C-LSSS S-B-LYYY- S-CLL-B-EE- YSS-SSSS SCC-Y-L LC-YYY- Y-LL C-B-EE- S-B-B-EE- YSS-B-S-L CCCC-CYY- S-B-LL Y-CCCL- Y-B-SEE-CCCCYY- E-EES-S-B-SSS- S-SLL-LL C-CCCE- C-B-B-EYYY- YCC-YYYY CCC-C-B-E- S-LYYY Y-B-EL- Y-EE C-EE EEEEL-EL- B(20/6) C(30/19) E(26/24) L(20/4) S(31/37) Y(31/22)  (LTLM=17, LA=30, homes=26) [16000 moves/s]
	// T 5280 6-Spliced (Score=6004057, COM=85, LR=5, ATW=267, music=96/18/20/7/6, LWM=119)  L-YYYY E-BEEEE-B-E- S-CCCCL-B-EE- S-B-LL C-CSSSSS- B(20/11) C(30/15) E(40/25) L(20/6) S(35/23) Y(20/16)  (LTLM=14, LA=29, homes=5)

	val calling6 = "HH BFIHxHxH HHxHxHx HHxHxHx HHxHxHx HHxHxHx"	// Anthony P Smith
	val calling7 = "HHxHx VxH HHxHx VxH HHxHx VxH HHxHx VxH HHxHx VxH"	// Michael S Bruce
	val calling8 = "I'W'F'F' W' I'H' B' MB'H' I'W'F'F'W'I'MMMH' B' MWH'H' WWB'H' I'W'F'F' W' I'H' B' MB'H'"	// Graham M Bradshaw
	val calling9 = "IsMs IsMsMs IsMs IsMsMs Fs MsWs MsWsWs MsWs MsWsWsBsHs"	// Anthony P Smith

	//val calling10 = "H H WWH WWHHH MMWH"	// A J Pitman - 5 part
	val calling10a = "H H VsVsWWH VsVsWWHHH MMWBsFsH"	// A J Pitman - 5 part
	// F(24) 5280 6-Spliced (Score=5996845, COM=90, LR=7, ATW=231, music=96/21/17/6/0, LWM=124)  B- B- SsBsSSS-YYY YYYY-SSS- CsBsSSS-B-L- B- B- E-B-C-SSsBsS- B(45/46) C(10/5) E(5/4) L(5/0) S(65/30) Y(35/11)  (LTLM=26, LA=26, homes=8)

	val calling10b = "H H WWH WWHHH MMWH  H H WWH WWHHH MMWH  H H WWH WWHHH MMWH  H H WWH WWHHH MMWH  H H WWH WWHHH MMWH"	// A J Pitman - 5 part

	val calling10c = "H H VsVsWWH VsVsWWHHH MMWBsFsH  H H VsVsWWH VsVsWWHHH MMWBsFsH  H H VsVsWWH VsVsWWHHH MMWBsFsH  H H VsVsWWH VsVsWWHHH MMWBsFsH  H H VsVsWWH VsVsWWHHH MMWBsFsH"	// A J Pitman - 5 part
	// F(8) 4512 6-Spliced (Score=5996415, COM=90, LR=7, ATW=230, music=89/22/16/1/0, LWM=106)  B- B- SsBsSSS-B-SSS- CsBsSSS-B-L- B- B- E-B-C-YYsBsS- YYYYYYY- B- SsBsSSS-B-SSS- CsBsSSS-B-L- B- B- E-B-C-YYsBsS- B- B- SsBsSSS-B-SSS- CsBsSSS-B-L- B- B- E-B-C-YYsBsS- B- B- SsBsSSS-B-SSS- CsBsSSS-B-L- B- B- E-B-C-YYsBsS- B- B- SsBsSSS-B-SSS- CsBsSSS-B-L- B- B- E-B-Y-YYsBsS- B(49/46) C(9/5) E(5/4) L(5/0) S(55/26) Y(18/8)  (LTLM=23, LA=33, homes=35)

	//val calling10 = "H H WWH WWHHH MMWH  H H WWH WWHHH MMWH  H H WWH WWHHH MMWH  H H WWH WWHHH MMWH  H H WWH WWHHH MMWH"	// A J Pitman - 5 part
	//val calling11 = "MMWWHH WWWH WHHH MWH WWWH W MMWW MH MMMH WHHH WWH MWWWHH BW MWHHH MW MB MMWWH MWHHH MW MMMWH MMHHH MWWH MMMHH" // A J Pitman
	val calling11 = "MMWWHH WWWH WHHH MWH WWWH W MMWW MH MMMH WHHH WWH MWWWHH BW MWHHH MW MB MMWWH MWHHH MW MMMWH MMHHH MWWH MMMHH" // A J Pitman
	val calling12 = "MMWW MH MWWBH WHH MWWWHH MWWWH WHHH MH MMMWHH MMMWHH MH B MMWH WHH WH MMMH MMWWH WWWH MMH BH BH WH" // A J Pitman
	//val calling13 = "H MMMWHH MMMWHH MH BMMWH W MMWW MH MWW BH WHH MWWWHH MWWWH WHHH WWH WWWH MMH BH BH WWH MMMH" // A J Pitman
	val calling13 = "HH MMMWHH MH BMMWH W MMWW MH MWW BH WHH M BH BH " // A J Pitman

	val calling14 = "M W H M H H W M M W H W B W B M B H W B W W B H M M H B H M H B H M W M H M B M H M M H"

	val ivin1 = "MW MW MW B MWH BWHH MWH BBH MWHH MBH MW B MW MW MWH BWHH BH BHH MBH"
	val ivin2 = "WH MBWH MW MW MWH MW BW B MB MW MW MWH MBWH MH MBWH MBWH MB MB MMW MWH BWHH MBWH"

	val dfm6part = "MWHH BHH WBBH MMH  MWHH BHH BBH MMH"	//
	val dfm5part = "MWWFI WWH MMVVVWH WHHH "

	val calling = dfm5part

	val methods = List(superlative, lessness, bristol, cambridge, london, yorkshire)
	//val methods = List(superlative, lessness, bristol, cornwall, london, yorkshire)
	//val methods = List(superlative, cornwall, yorkshire, bristol)

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	//override def acceptNode(node: Node, leads: List[Lead]) = node.methods.forall(_==bristol) || node.music(0)>0

	def scoreFn(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*200 +
				comp.atwScore*0 -
				comp.com*7 +
				ScoreFactory.leastPopularMethodCount(comp)*50 +
				ScoreFactory.balanceScore(comp)*20 +
				ScoreFactory.strictLenScore(comp)*2 +
				comp.music(0)+comp.music(1)*2

	override lazy val musicDefs =
	{
		Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens())
	}

	override lazy val calls = List(Major.Bob, Call('s', 'S', "s", PN("1234")), Call('\'', 'Z', "'", PN("1256")), Major.BigBob)

	/** Laminated only - no splicing within nodes */
	//override def acceptSplice(splice: Splice) = splice.methodsUsed.size==1
	//override def acceptSplice(splice: Splice) = splice.com<2
}