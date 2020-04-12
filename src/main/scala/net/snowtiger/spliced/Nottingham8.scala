package net.snowtiger.spliced

import net.snowtiger.ringing.{Music, NamedMethod, Row}
import net.snowtiger.spliced.composition.{Composition, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * For Alan Reading. Also re-purposed to do the PPE Core 7, and my Renaissance 7.
 * @author mark
 */
object Nottingham8 extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	val cassiobury = NamedMethod("K Cassiobury", 8, "-58-16-12-36-12-58-14-18", "12")			// c, B

	val calling_orig = 		"MBH WWBHHH FWH VVVWH IWH BFIH FIH IBFWBFIVFBMMVVVH BFIH FIBWBFIWMMMWH MFIFIWWWBHHH"
	val calling_r1 = 			"MBH WWBHHH FWH VVVWH IWHH FIH IBFWBFIVFBMMVVVH BFIH FIBWWMMMWH MFIFIWWWBHHH"
	val calling_r1_ext = 	"MBH WWBHHH FWH VVVWH IWHH FIH IBFWBFIVFBMMVVVH BFIH FIBWWMMMWH MFWWWIFIWWWBHHH"
	val calling_r2 = 			"MBH WWBHHH FWH VVVWH IWHH FIH    W   VFBMMVVVH    H    WWMMMWH MFIFIWWWBHHH"

	val calling_r3 = 			"MH IBF WWM FWWW IF IWWW BWHH WBHHH MH WVBFI FFWH VVVWH IWMH WMM WHH WMH WHHH"

	val calling = calling_r3

	val methods = List(cassiobury, cornwall, glasgow, superlative, cambridge, london, lessness, bristol)
	// Now with the Core 7
	//val methods = List(yorkshire, cornwall, superlative, cambridge, london, lessness, bristol)
	// Now with Renaissance methods
	//val methods = List(malpas, superlative, adelaide, deva, cornwall, lessness, bristol)

	override val seed: Option[String] = Some("nottingham8/nottingham8_r3.txt")
	//override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	// Core 7 - calling_r1_ext
	// T 5088 7-Spliced (Score=7011633, COM=123, LR=4, ATW=343, music=237/48/45/17/0/1, LWM=61)  O-BO-SS- EY-L EC-E SS-YS- B- CEO- ESS-OBL-B B- C-LCO-B-E-E- EO-CS-L- LYE- SSSCY-B-OO B- EE-YLL-LLC-EOL-B BEL-YY YYL-B-OB-CL-O-OE O-B-EOE-B-YEE-EE- CS-B-Y SSSO-SCE- EOO-B-B-EO-B-L L-LY O-OY-C-L- E-OO-OBE-YECYC-CYC SE-OL-O-B-YC-CYS ES-YCS CO-SCO-OO- B- LYE- B(21/50) C(20/9) E(29/56) L(19/11) O(29/62) S(21/32) Y(20/17)  (LTLM=9, LA=47, homes=28) ATW
	// Core 7 - calling_r2
	// T 5024 7-Spliced (Score=7011649, COM=124, LR=3, ATW=343, music=212/56/36/20/0/0, LWM=70)  L-BL-CS- SO-CYY CL-SYO-CS- SCYYCCY- CSYSE- SSO-OBBB-E- OBBO-CYYYL-B-E-YYS- BO-SC-L- B- LSC-LELEL EL-LLEO- YYSC-E EOBBB-LOEEL-O-YES-LY SYC-OBO-LCL-CEE-EO- B- CYSC-OS-E CYS-EES-YSSC CCY-C-YYC- O-SS-B-O-B-SS-E EY-OBBOO-L CC-ELE- B- EYO- B(19/35) C(25/23) E(23/39) L(19/14) O(21/42) S(25/35) Y(25/24)  (LTLM=28, LA=47, homes=23) ATW
	// T 5088 7-Spliced (Score=7011644, COM=132, LR=3, ATW=343, music=218/58/39/15/1/1, LWM=72)  O-BE-YS- EC-SYY SYSY-SYO-YS- SCCSSSC- YSYCL- SES-OBO-YLO B- LOBE-CSCYE-B-O-B B- BO-YS-L- CYCEC- LSY-ELLEOL-OEO- YYYC-B OOBB-LE OEL-O-CY E-LEOBB O-EOO-O BOLL-CEE-EO- SLE- LC-CYESS-E L-B-CSSC O-Y-CCC- E-SS-B-O-B-YS-E EY-OY-SCL-OE- ECL- LSE- B(17/27) C(22/23) E(26/57) L(19/7) O(26/44) S(26/36) Y(23/24)  (LTLM=30, LA=48, homes=26) ATW

	// Core 7 - calling_r3
	// T 5088 7-Spliced (Score=7011672, COM=125, LR=4, ATW=343, music=244/64/47/17/0/0, LWM=65)  O-BBOO B- EL-EYE-LO ELEL-ELEE-E SL-B L-OO-OBO-OC-L ES-OO-O-B-YC-YEL-YSS SYYS-SCE-EE-E- SEE- OLOBB-CYS CS-OL- B- CSSES- O-YO- CCCC-OBBOB-C-LYE-B-L-BO-OEE-L- LOBO-O BELEO-YYEY Y-E-YSC- BO-SY-B E-YSYC- SL-B BB-B-S-CYC- ESO- CL-CEYY-CE- LS-O- OC- B- B(23/41) C(19/16) E(29/48) L(19/12) O(29/82) S(20/29) Y(20/16)  (LTLM=32, LA=42, homes=27) ATW
	// T 5088 7-Spliced (Score=7011684, COM=122, LR=4, ATW=343, music=238/63/47/21/0/0, LWM=67)  O-BC- EO-CSCYC YY-SS CYL-ELLO-YLL-L L-SS-OEL-YLYSS-L SL-L EO-O-OC-YY-B-CYESC-YCE-EO-E- LEOBEL- BBOBBB-CYO-OO- B- B- O-LS- CE-OOBBB-C-EEY-B-L-BO-OEE-L- C-O EELEO-SYLY Y-CCY-SSC- EO-CY-B E-SE- YO-B BB-B-S-CYS- ECO- BY-B BB-SCYC- CYCS-O- B- CSSCO- B(25/51) C(25/31) E(22/21) L(19/15) O(25/84) S(20/16) Y(23/20)  (LTLM=16, LA=50, homes=26) ATW
	// T 5088 7-Spliced (Score=7011689, COM=127, LR=4, ATW=343, music=247/61/45/22/1/0, LWM=66)  O-BELLO B- BL-EYE-SC CSCSY-EELO-SYEYS-L O-SS-OEL-CLE-L OELEL-OO-O-B-YC-OC-CYC EY-YSE-EE-O- LEC- EEOBL-YYO-OE- B- B- O-CYYC- YCSS-CL-S-B-B-E-BO-OEO-CCS- C-O S-CSLY Y-YCY-B B- BO-CY-B E-LC- BBLEEL-B BB-B-S-CYS- LSE- BC-B SYC-OBBBO- LS-O- LSO- ESO- B(25/48) C(23/20) E(25/33) L(20/17) O(25/84) S(21/20) Y(20/25)  (LTLM=13, LA=40, homes=27) ATW
	// T 5088 7-Spliced (Score=7011691, COM=125, LR=4, ATW=343, music=249/61/45/22/1/0, LWM=65)  O-EELLO B- BL-EYE-SC CSCSY-EELO-SYEYS-L O-SS-OEL-CLE-L OLLEO-OO-O-B-YC-OY-CYC EY-YSE-EE-O- LEC- EEOBL-YYO-OE- B- B- O-CYYC- YCSS-CL-S-B-B-E-BO-OEO-CCS- C-O S-CSLY Y-YCY-B B- BO-CY-B E-LC- BBLEEL-B BB-B-S-CYS- LSE- BC-B SYC-OBBBO- LS-O- LSO- ESO- B(24/48) C(22/20) E(25/33) L(20/17) O(26/86) S(21/20) Y(21/25)  (LTLM=13, LA=40, homes=27) ATW
	// T 5088 7-Spliced (Score=7011693, COM=125, LR=4, ATW=343, music=249/62/44/18/1/0, LWM=60)  E-BC- BO-EYL-SC YSE-EOBB-SLL-L L-YS-OEL-CLCCS-L ES-E EO-O-B-YC-OC-CYECY-YSE-EO-E- SYLCC- LLOEE-YYO-OO- B- OC- O-LY- YE-YSS Y-Y-YEYSC-EYE-SCS-BO-OBE-L- C-SYCYL-O Y-YCC-B B- EO-CY-B E-BY- YL-B BB-OBOE L-S-O- CLO- BC-E L-BEOBB B- CSSS-O- LSO- CSSSO- B(21/33) C(23/20) E(24/39) L(19/16) O(25/82) S(22/28) Y(25/31)  (LTLM=8, LA=38, homes=28) ATW
	// T 5056 7-Spliced (Score=7011698, COM=123, LR=3, ATW=343, music=245/64/51/20/1/1, LWM=62)  E-BBOO B- EL-ESY YS-LSSYS-EEEO-SLL-B L-SS-OBL-L LC-SEO-E EO-O-B-YS-B-OY-YCE-EO-O- LCL- LY-CCO-OO- B- LSE- E-BC- YL-OBBOB-C-YLYSS-C CLSY-L-BO-OEL-B B- EOEO-SSES S-O LOBE-O-B B- BO-CS-B E-LC- EY-B BB-YE O-S-YSY- SCCSE- BC-YEYC-YCYC- YYCS-O- B- CEE- B(25/38) C(19/22) E(25/47) L(19/15) O(25/72) S(24/32) Y(21/19)  (LTLM=34, LA=38, homes=29) ATW
	// T 5056 7-Spliced (Score=7011700, COM=125, LR=3, ATW=343, music=247/64/51/20/1/1, LWM=61)  E-BBOO B- EL-ESY YS-LYCYS-EEEO-SLL-B L-SS-OBL-L LC-SEO-E EO-O-B-YS-B-OY-YCE-EO-O- LCL- LS-CCO-OO- B- LSE- E-BC- YL-OBBOB-C-YLYSY-C CLSY-L-BO-OEL-B B- EOEO-SSES S-O LOBE-O-B B- BO-CS-B E-LC- EY-B BB-YE O-S-YSY- SCCSE- BC-YEYC-YCYC- YYCS-O- B- CEE- B(25/38) C(20/24) E(25/47) L(19/15) O(25/72) S(22/29) Y(22/22)  (LTLM=15, LA=38, homes=29) ATW
	// T 5056 7-Spliced (Score=7011710, COM=131, LR=3, ATW=343, music=257/64/45/18/1/1, LWM=60)  O-LS- EL-ESY CS-CC CYO-EEEO-B-B L-YS-OBO-SLCYY-L SE-OO-O-B-YC-YSLSC-YLYCC-SCE-EO-B EEC- LSE- BBOEE-SCO-OO- B- LES- E-CYSC- YYCY-OLELE-S-CLYYC-C CLSY-E-BO-OEL-B B- LOBO-SYES C-B-E-B B- BO-YY-B E-SYCY- BS-OB-B-S-YSC- SLO- CL-B BB-YO- LS-O- B- OS- B(24/45) C(23/23) E(22/35) L(18/16) O(24/84) S(24/32) Y(23/22)  (LTLM=19, LA=33, homes=27) ATW

	// Rennaisance 7 - calling_r3
	// T 5088 7-Spliced (Score=7011741, COM=135, LR=3, ATW=343, music=280/68/55/20/1/1, LWM=68)  C-SC- LL-A LDAL-CL CLM-LCL-ACAA-AS-SS-CAS-SAS-B-D-C-B-CC-DSS-B-CLL-SA-SD- SSSLS- BLDB-A-D- AD- B- SD-MBMM- DBM-B BMBL-AS D-SAL A-DMM-L-AS-DAD-B B- DM-DLD AC-DCL-M-B B- ADC-D-MAM L-LCCM- BBLMM-DA M-B-CBBDA-M- DCM- BAC-MCCM M-DBC- MMLM-C- B- SDAM- A(23/25) B(23/49) C(23/52) D(24/65) L(21/25) M(25/37) S(20/27)  (LTLM=32, LA=66, homes=27) ATW
	// T 5088 7-Spliced (Score=7011761, COM=126, LR=3, ATW=343, music=296/70/57/20/1/1, LWM=66)  SD-LLLCL- BDA-D CL-CMMM-DB-L SL-DS-SS-CAS-L CA-MS-D-C-B-CC-SAS-CMBBDA-MLL-SA-AA- MS- LASD-A-D- LAAAC- ASS- M-LAM- MMBBB-SSLD S-M D-MB BD-B-M-AS-DB-B B- DC-DMDMD S-DML-L-B B- DDC-CM-AS-CLCM- AACD-DA C-B-LLD-M- MDC- DLL-CMCC M-BBA- MA-M- CBA- CDC- A(23/41) B(20/26) C(24/40) D(26/61) L(21/39) M(25/52) S(20/37)  (LTLM=14, LA=40, homes=27) ATW
	// T 5088 7-Spliced (Score=7011772, COM=133, LR=3, ATW=343, music=311/68/65/22/1/1, LWM=64)  C-MBBLM- LL-MBMBL-SAL LASM-LMC-L SM-AS-SS-A-L MA-MDC-D-C-B-CC-SAS-CLA-CLL-SA-SD- B- BLDB-A-D- LALAA- B- SSS-MBMM- LLMBM-MMCB-AL-MDSD-DD-M-AS-DAD-B B- DC-DADBC-LDSA-C-B B- BC-D-AA L-LS- ADBA-DS-B-CCDA-M- DCL- SM-MCLLC M-SC- MCDS-C- LSDA- CDC- A(23/39) B(20/35) C(23/58) D(23/64) L(25/42) M(23/37) S(22/36)  (LTLM=35, LA=41, homes=23) ATW
	// T 5088 7-Spliced (Score=7011779, COM=129, LR=3, ATW=343, music=308/73/57/22/1/1, LWM=62)  SD-MLCB B- LM-D ASL-SAL SSL-LMM-L ASLSS-AS-SS-CDS-SLL-MDC-D-C-B-CC-SAS-CS-MLC-SA-AA- MS- CDB-A-D- LLS- B- M-BBBD- MMBL-B DL-AL-DD-CAM-L-AS-DB-B B- DM-DAMCLL-DCDA-L-B B- DDC-CM-AA L-CCBM- LMD-DA DA-B-AC-B B- DCC- DLL-CLMLC M-MA- MCMB-M- CBA- CAC- A(22/42) B(21/36) C(25/40) D(23/54) L(26/49) M(22/49) S(20/38)  (LTLM=10, LA=69, homes=27) ATW
	// T 5088 7-Spliced (Score=7011778, COM=132, LR=3, ATW=343, music=309/72/59/22/1/1, LWM=66)  C-MLCM- AMD-D ASL-LSL-A-MMCM-SLSS-CC-A-B-MCBBDA-SS-C-B-CC-CAM-ALLAS LS-CBC-LAA-B LMA- SDS- CDAML-A-D- B- ASAM- L-MMBC- CLCM-AMAD-M D-MDD BD-DD-M-BC-DAD-B B- CD-DD-B-C-B B- BDA-CAA-B M-SSD- MCDS-B BB-AD-AL-L- B- CMBC-SLAS L-SC- LDM-C- B- SSLALS- A(25/48) B(23/43) C(25/52) D(23/73) L(20/31) M(22/24) S(21/38)  (LTLM=27, LA=53, homes=26) ATW
	// T 5056 7-Spliced (Score=7011783, COM=114, LR=3, ATW=343, music=308/75/51/22/1/1, LWM=64)  C-BBBMC- BL-D D-DMM-DB-L CLLLM-MASS-CC-A-B-MMD-SDC-C-B-SS-SAAM-AA SDA-SSS D-LM-AA- AML- BADA-A-CC- SLSSS- CLCLM- L-LLD B- ALL-DBBB-M D-LM BD-DD-L-BC-A-B B- AC-AD-DCL-C-B B- DDC-CAA-ACD-DDS- MLD-SA-AC C-LMCB-L- MS- CMBM-L C-BS- CCDMML-C- B- SDS- A(22/55) B(22/28) C(26/74) D(25/59) L(23/38) M(20/12) S(20/42)  (LTLM=23, LA=53, homes=27) ATW


	//override val seed: Option[String] = Some("T 5088 7-Spliced (Score=7011772, COM=133, LR=3, ATW=343, music=311/68/65/22/1/1, LWM=64)  C-MBBLM- LL-MBMBL-SAL LASM-LMC-L SM-AS-SS-A-L MA-MDC-D-C-B-CC-SAS-CLA-CLL-SA-SD- B- BLDB-A-D- LALAA- B- SSS-MBMM- LLMBM-MMCB-AL-MDSD-DD-M-AS-DAD-B B- DC-DADBC-LDSA-C-B B- BC-D-AA L-LS- ADBA-DS-B-CCDA-M- DCL- SM-MCLLC M-SC- MCDS-C- LSDA- CDC- A(23/39) B(20/35) C(23/58) D(23/64) L(25/42) M(23/37) S(22/36)  (LTLM=35, LA=41, homes=23) ATW")

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	//override def acceptNode(node: Node, leads: List[Lead]) = node.methods.forall(_==bristol) || node.music(0)>0

	def scoreFn(comp: Composition) = atwScore(comp)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*300 +
				comp.atwScore*30 +
				ScoreFactory.balanceScore(comp)*5 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*5 +
				(if (comp.isAtw) 50 else 0) +
				2*(comp.music(0)+comp.music(1)*2+comp.music(6)*20)

	def balanceScore(comp: Composition) =
		comp.methodsUsed.size*1000000 -
				comp.falseScore*300 +
				comp.atwScore*20 +
				ScoreFactory.balanceScore(comp)*40 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*3 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+comp.music(1)

	def noComScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*25 +
				ScoreFactory.balanceScore(comp)*5 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*30 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+comp.music(1)

	val backRounds = new Music()
	{
		override def countMusic(row: Row) = if (row.reverse.isRounds) 1 else 0
	}
	override lazy val musicDefs =
	{
		Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueensRow(), new MusicWhittingtonsRow(), backRounds)
	}

	override lazy val calls = List(Major.Bob)

	val lessnessOrCornwall = Set(lessness, cornwall)

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		/*
		if (node.startLH == Row("16532478"))
		{
			val inSeed = node.methodsUsed == Set(getCallingMethod)
			inSeed || node.methods.size<=2
		}
		else if (node.startLH == Row("15324786") || node.startLH == Row("17234586"))
		{
			val inSeed = node.methodsUsed == Set(getCallingMethod)
			inSeed
		}
		else if (node.startLH == Row("14238765"))
		{
			val inSeed = node.methodsUsed == Set(getCallingMethod)
			val i = leads.map(_.startLH).indexOf(Row("15763842"))
			inSeed || (i>=0 && lessnessOrCornwall.contains(node.methods(i)))
		}
		else*/
			super.acceptNode(node, leads)
	}
}