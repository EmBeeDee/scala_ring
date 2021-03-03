package net.snowtiger.spliced

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition.{Composition, CompositionPlan, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * @author mark
 */

object PPE_Core7 extends SplicedGenerator with SearchDefinitionBase with StandardMethods
{
	val callings = Map(
		"Middletons" -> "MMWWHHH "*5,		// ATW=392, music=127/34/26/3 - seed from Yorkshire
		"GACJ NCR" -> "BBMH WHH WWBH W"*5,
		"Morrison" -> "WM BBH BWH WM BBH BW "*3,	// False with all seeds

		"Seed 191" ->  "B M HW B BW B MW B B B HB MB M HHW B MB HB HB B HHWW B M HW B H",		// T 5024 7-Spliced (Score=7014480, COM=119, LR=3, ATW=343, music=179/38/36/11/2, LWM=88)  EEE-BSS-CE- CO-SSY YS-BBL ELO-BO-L SS-SC L-ELEEL-SES-EO BBO-EEOOB-BLL- BEO-YEOBOB-EO-YY CYY-OBOL- OC- LS-OOLEO OL-SEOEL L-LE-YS- EYYLS-EYYYO- BCCEC-BS-BBE- CYCCSYS- SCCC-E OBBBL-YSC SY-CS YYC-OLEO OC- BBBLOB-SSS CC-SC- B(23/51) C(21/24) E(25/29) L(19/16) O(25/14) S(25/33) Y(19/12)  (LTLM=17, LA=40, homes=23) ATW
	 	"Seed 190.2" -> "B M HW B BW B MW B B B HB MB M HHW B MB HB HB B HHMB B M HW B H",
		"Seed 189" -> "B BW B M HB B HW B BW B MW B B B HB MB M HHW B MB HB HB B HHM H",
		"Seed 192.2" -> "B M HW MW B MB HBWW MW BW B MW B B B HB MB M HW B B HHWW B M HB HH",
		"Seed 198.1" -> "MB HW B HB B B B HHWW B MMB MB MB HB B B BW B MMB HHMMB B HHM HMB M HW B H",
		"Seed 198.2" -> "MB HW B HB HB BW B B HHWW B MMB MB MB HB B B BW B MMB HHMB HM HMB M HW B H",
		"Seed 196" -> "M HMBWW B B BW B MMB MB B B HHWW B MMBWW M HW B HB HB B HHM HMB M HW B H",

		"Seed 193.1" -> "B B B MB MMB MB M HW B B HHMB HM HMB M HWW BW B MW B B B B HBWW HW HH",		// T 5024 7-Spliced (Score=7014518, COM=128, LR=3, ATW=343, music=195/55/37/9/2, LWM=85)  BOB-SY YS-YY CC-SC YCS-BBB-OOLEC-OELLE L-YCYE-OOOS-BO-CEOBB L-SE- BY-B BBO-SC YC-OE- CSSCL- SCS-OB-BBE- CEYYEC-OLEEO- E-OB-BO BELC-LC- BC-SYS SYYC-E SY-BL-L LOOS-CC L-EOBE-B BLE-BO BEE-OE OO-BS-EO LEC- SESSE-SS EC-B-O- YO-E- OS- B(27/54) C(22/20) E(27/29) L(15/7) O(27/34) S(23/28) Y(16/23)  (LTLM=22, LA=46, homes=27) ATW
		"Seed 193.2" -> "W B M HHB HB B MW MMB HB B HB HHW B MB B B B B HB MB MB HB B BWW B H",		// T 5024 7-Spliced (Score=7014483, COM=131, LR=3, ATW=343, music=190/46/37/17/0, LWM=90)  OELOOY-SES-OOEEOO E-SCSY- CYECS- BOB-BO B- OBOS-OE LOOY-BBB E-Y-L SCS-B-YYLS-BLL- CEOEO-YS YC-EO OY- BOB-BLE- B- BC-B EOB-SC SYS-OB-BEE ELE-BEOOB-SY SY-YY CS-BBO- SEOEL-SC E-OB-ESY-EO-LCCYE- SC-OE ELO-BEL LEE-YS SE-OS-B SS-OE- B(26/45) C(13/7) E(31/29) L(13/9) O(31/46) S(25/37) Y(18/17)  (LTLM=25, LA=35, homes=27) ATW
		"Seed 193.3" -> "HB MW MMB HB B HB HHW B MB B B B B HB MB MB HB B B BW B M HHB HW B H",   // T 5088 7-Spliced (Score=7014506, COM=130, LR=3, ATW=343, music=199/49/34/14/1, LWM=89)  B- BYSCE-SS L-C-L SSS-B-CYES-BYYCO- LEE-YY YC-OE- BOB-BBO- ESO- SYCS-OOB-SEOBE L-EL-OE LOB-BLOEO-YC CS-SE-EEO- BEL-EEOB-OOS-YLY-EO-LCCES- OBOS-BO EOB-BBE LEO-YS YS-BO-SCY YS-OOLEOO L-CCCY- CSECY- BBBOC-BO LEC- SE-B EOLEC-LEE- B(25/57) C(19/21) E(30/32) L(16/11) O(30/38) S(22/30) Y(17/10)  (LTLM=23, LA=30, homes=24) ATW
		"Seed 192.3" -> "B M HMMB MM HB HW B HHB HHMBWW B B BW B MW B HM HHW B MM HHB M HW B H",	// T 5024 7-Spliced (Score=7014477, COM=127, LR=3, ATW=343, music=178/42/35/12/0, LWM=83)  EEE-SC SCS-SE- CSC-YL O-LO-SY E-OY-EY- LSSYL-BLL- LOBBE-SCY CS-YS- CEE- YEYSO-OE- EOOEL- YYS-OB-BBOC-OC-B CY-YS YS-BEL OBB-EO-E ELL-SC O-OBBE-OOLEC-BBO- CECSLC-EOLO B- B- LEEOE-L OBOS-SY L-B-BY- OC- OBB-SY SYC-OEOE- BC-SYS SS-BBO- B(24/26) C(18/14) E(26/28) L(17/15) O(28/30) S(25/41) Y(19/24)  (LTLM=30, LA=52, homes=28) ATW

		"Seed 162 singles" -> "B M HW B BW B H'W'W M HB HHW HB B B B BWW B MM HHB HW B H",			// 392, music=140/32/25/13
		"Bristol 5120" -> "H W BH MBBBBBH B MH BH W B MMH MW MW MMBWW MW MWW MH",		// T 5024 7-Spliced (Score=7014474, COM=127, LR=3, ATW=343, music=173/43/38/12/0, LWM=91)  B- LOLEO-SSC YY-EYCLC- SEOB-EO-SS CY-OL OBOS-CS CC-BBB LEL-BBE- CLOBBB-OB L-YCSS- LYCSE-BSYCO- BY-OBL-BEE L-CYSY YCY-BBLOB B- O-EC SO-YCY SCC-OOBB-E CYS-EY E-OEES-SY SSCS-SEL-OEEOO E-OOE-CYC YYY-OBLE-YEE-CCY CCS-OOBE- B(24/38) C(25/16) E(24/40) L(14/9) O(25/26) S(22/36) Y(23/8)  (LTLM=9, LA=38, homes=24) ATW
		"Bristol 5088 no.7" -> "H W BH MB MMH BH W BH'H MM'WH BBBWH BW BBBH BHH MBBWW MH",	// T 5024 7-Spliced (Score=7014478, COM=126, LR=3, ATW=343, music=189/39/35/19/0, LWM=84)  OS- CE-L YS-BBL- O-OOY-SY YSS-B-CYSS- EOEEC-EYCES- BC-YSY SS-ESCYO' YSCYYYC- O-SSCS CCY'OOE-CLSCLS- EOB-EO EOB-BLL OBB-EO-CCS- CS-SLOBE LY-L OBLES-BEE ELE-BO BOB-OO- SS-BBE- LEOEEL- CEOLEC-OB-BEL OO-BE-YSS SE-B SCC-OBBEO- B(23/58) C(20/14) E(28/15) L(15/8) O(27/40) S(28/40) Y(16/14)  (LTLM=7, LA=56, homes=25) ATW
		"Bristol 5120 no.4" -> "BBBBWH MH B MMBH MMH WW BW BBBHH WW' M'MB MMH W MMH MB",	// T 5024 7-Spliced (Score=7014481, COM=117, LR=3, ATW=343, music=172/46/36/16/0, LWM=92)  CC-SY SY-EEL ELO-BEE OBB-BO-B ELC- YLSSEY-CCSC- SESSO-YY YSS-SE O-EE-OL- BOC-SCSC YCC-OOEE- BBBLLE-CLYYY-L CC-YY LS-OOLEY-BBB LOB-BO BEO-BEO- YEO- EOBEO-E YL'B BB'CYCY YCY-BBB-SY L-EEOO L-BLEOE- BOOL-CSS YYC-B-BC- O-EE-YC B(27/39) C(21/16) E(27/14) L(17/12) O(24/52) S(17/18) Y(24/21)  (LTLM=12, LA=44, homes=26) ATW
		"Bristol 5152 no.4" -> "BBBBWWH MB MBHH MBH BH BHH B MMB MMHH MH WW BW BBWW BWHH B",	// T 5024 7-Spliced (Score=7014480, COM=127, LR=3, ATW=343, music=185/46/33/17/1, LWM=93)  SS-EY-ELL LOB-OE OE-OOC-B-O- CLYYEY-OB-BO BOS-CL OBB-CEOO B- EEC- CYC-OEEC-OE- EEO-BBE- BOB-BO B- OY- CS-YC O-OLEEE O-EO-BYY-LLC-BY- EOBLEL- YCS-OOBL- OBOBOY-SYS YCYS-E ELE-LSSY-SSO-BEL EOB-BE-B-SCY YS-EE-CESSES- SEL- OBOS-CS B(25/74) C(16/5) E(31/26) L(15/12) O(32/38) S(20/17) Y(18/13)  (LTLM=16, LA=41, homes=25) ATW
		"Bristol 5152 no.3" -> "BWH WW MHH B MMW MW BB MBBBBBHH MH WW BH MBBBB",		// T 5024 7-Spliced (Score=7014422, COM=118, LR=4, ATW=343, music=154/40/32/10/0, LWM=97)  EEO-BBB-CLYCLY- YSYC-B-OLELLO O-OOEL- B- BYSLC-BEL O-LC O-BBLLE-B YSC-BBEEE-L SY-YC YC-CY CSC-EE-OE LEO-BO EEO-BEOBBB-OE LEO-BBL- SSCYYYS- CLCSEC-EEOEO- YSYY-CYS SSCS-OOOS-CLSCO- SEOOC-OB-BO BOB-OE OBB-BBL EEE-BEE B(28/44) C(20/10) E(29/30) L(18/5) O(28/42) S(17/9) Y(17/14)  (LTLM=13, LA=46, homes=22) ATW

		"Seed 1" -> "B B B M HWW B B B MB B HW B BWW HMB MB HB HB B MB MB B MB B M HH ",
		"Seed 2" -> "B B B M HMB B B HB BW BW B B HB MB B MW B B B MB MB B MMW BWW BW ",   //BAD!
		"Seed 8" -> "M HBW HM HMB B HMW B MB B MW MW B B BW B MB BW HB B B BW B M HW ",	//BAD!
		"Seed long" -> "B B B MB HW B HB B B MB B M HB B HB B B B BW B BWW B B B MB B MB B B B HW ",
		"Seed short" -> "B B B M HMW B B HB MB B MW B B B MB MB B MMW BWW HM H",

		"B1" -> "B B W WH B W MW MH B W MH H M MW WH H M M BH H WH M B WH W W BH WH MH W W B",
		"C1" -> "W MWBWH H MH W MW MBMH BH W WBH M MW WH H W W M MH H M MBH M MW WH H ",

		"new1" -> "B M H W B W B H M B B M B H H W W H B W B B B B W H H W",
		"new2" -> "M H W B M W W H M B H B H B M M W W B B W B M H H M H B H W W B",

		// T 5056 7-Spliced (Score=7015065, COM=130, LR=3, ATW=343, music=228/59/39/20/2, LWM=76)  OBB-EO ELO-SC E-OELEL- CSY-CYES-CO-YC EY-E LY-L- BOOO-SSO-LO EEY- YYC-CCEYS-OOEO- LYSSC- CESCEY-OLLO B- CCSLC- YL-B YYLCC- E-LC CO-E OBOS-ESS-OLEO E-BOOB B- SECSY- SCS-SL E-LO-CEOEL- BB-YE- CEOEE-SLOBL- SCS-B-CYYE-SS O-OOS-BBE B(15/18) C(24/19) E(29/61) L(19/10) O(29/52) S(24/51) Y(18/17)  (LTLM=18, LA=39, homes=27) ATW
		// T 5024 7-Spliced (Score=7014540, COM=132, LR=3, ATW=343, music=203/60/38/12/2, LWM=79)  OBB-BC-BO O-CYCS- BOS-ES CS-YC SS-YY YCCY-E OBBEE-B OS- EEOEL-OLO-CY- L-YCSC SYC-OLOE- SSLSY- CSY-LOEO B- SYSLS- BC-O- E-EC YE-E EOB-ESS-OEOL O-LOOE- SYCCYYS- O-SSSC E-LY CS-CEOEL- CYS-OLLEE- LCYSE-YLYCL- O-B-EO-BBB O-CL YS-BBE B(17/24) C(23/15) E(25/44) L(17/9) O(26/40) S(27/48) Y(22/23)  (LTLM=42, LA=54, homes=29) ATW
		"new3" -> "B B M H M B B W W H W B H M M H H M H H W H M W B M M H H M M B H M H B H M M B M B",

		"new4" -> "B M M H H M M B H M H B H M H B W W H W B M H M B M M H H M M B H M H B H M M B M B",	// T 5088 7-Spliced (Score=7014508, COM=138, LR=2, ATW=343, music=186/49/35/15/1, LWM=85)  EOOY-SS YYS-B-ES- B- YECCLS-OLOB E-CYLY-BEL- BOS-ES- BOLEC-BO LEC- L-BBOBO- BOB-SS EY-B-E- BOOBB-OOEEC-YEOBE L-CL- CEOOS-ES CS-BBOB-YCYS CYC-CL- YEO- BSSECS-YSYS O-EO-BBL- YESSLS-CL- BEE-BEL- SLOLLY-OC-CSLY-BO BOS-CSSY CS-OE B(27/42) C(19/14) E(24/36) L(18/10) O(26/20) S(27/42) Y(18/22)  (LTLM=17, LA=31, homes=24) ATW
		"new5" -> "B H B H M H B H M H B W W H W B M H H M H H M H B M M H H M M B H M H B H M M B M B",	// T 5088 7-Spliced (Score=7014511, COM=133, LR=3, ATW=343, music=184/55/34/11/2, LWM=89)  OBOC-SC- LCSSO-BCYCL- BB-BS- OBOC-SESCO- L-OEEEO- SS-YC LY-YEE-O- EEEOO-B BEO-SEY-OBBBL- YCSLY- E-ES- B- YCY-OLLO B- OBLES-EO E-CE CYC-OLOB B- SEO- CESCLS-OOO E-SCLY-BBL- CECSEY-CYSY- CS-BYSLC- YESSLY-CYYS L-OB-BLE E-CSCS SS-OB B(22/23) C(24/8) E(25/39) L(17/9) O(25/46) S(27/43) Y(19/16)  (LTLM=25, LA=37, homes=27) ATW
		"new6" -> "B H B H M H W H H M H B W H H W H M H B W W H W B B M H H M M B H M H B H M M B M B",	// T 5056 7-Spliced (Score=7014521, COM=132, LR=2, ATW=343, music=193/49/40/11/2, LWM=84)  BOOC-BO B- SEYSO-BYSCL- YLOEEY-ES- OBOE-SCC- LOLOL- O-SE- EYSES-SC LY-E- SYCCYSC- LOBBL-CLYSLS- L-SE- LCYEY-CY LY-E OLEEO-B B- EOBBO-B BEO-YY SS-BBOB-YE- SSCCYSS- CLCCEY-SE L-EO-CS- L-LOBEE- BEO-EYCCO- L-ES O-OB-BLOOC-YSSE-OB B(21/50) C(20/12) E(26/40) L(20/16) O(26/24) S(25/34) Y(20/17)  (LTLM=16, LA=33, homes=26) ATW
		"new7" -> "M H M B M M H H M M H M M B H M H B H M H B W W H W B B M M H B H M H B H M M B M B",		// T 5024 7-Spliced (Score=7014497, COM=125, LR=3, ATW=343, music=184/46/34/10/0, LWM=88)  L-OBBEO- BB-LY CS-SS L-OS-ES- SYCLS- YSY-YSCY E-CL- CEYSLC-OY-EL-OB B- BOY-SL- EOB-BEO- O-OLEO B- BOB-OL OBOL-OC-YLO B- BOELO-B OO-ES-EEOB-CE CCS-SE- EYYES-BO OC- CEYYEC-OLLEE- CESCL-BBB ELC- YLCSEY-YCCEC-BBB-SS E-CYES-CLOBB B(26/34) C(22/18) E(26/32) L(19/24) O(26/34) S(20/28) Y(18/14)  (LTLM=11, LA=39, homes=26) ATW

		"agr1" -> "MH WW BMWH BMM BBBW BHH MHH MW' MWHH WH' MWW BWW BHH WMH WHHH",
		"agr2" -> "MH WW BMWH BMM BBBW BHH MHH MW MWHH WH MWW BWW BHH WMH WHHH",
		"agr3" -> "MH WWBM WH BM WWW MBBB WBHH MHH MW MWHH WB W WH WWWB WWBHH WMH WHHH"
	)

	val callings44 = List(
		"B B B H M W B W H M H H M M B H M B B M B H W H B B W B M H H M M B H M H B H M M B M B",	// 0 209 T 5024 7-Spliced (Score=7014532, COM=129, LR=3, ATW=343, music=201/53/42/13/1, LWM=82)  CC-SY SS-BEOOB-BO OC- L-OLEL-E EEL-OE LS-O- SEYSEC-ES- ECL- O-OY-YSYY CY-CC- BB-SCSY CS-YY CS-BBL E-CL SC-YS- BLEOO-CYC- EEO-BS-BL-E YS-OB O-LELEEO- EOBEEE- L-YCSS CSS-OB-BO B- L-BBELO B- LYSYE-BO OC- L-YL O-CL CC-BLOELC-YSSS YS-SY B(20/27) C(22/17) E(25/46) L(21/13) O(23/34) S(26/50) Y(20/14)  (LTLM=15, LA=40, homes=31) ATW
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
		"B H B H W W B W H M M W H B B H H B W H B H W H B B W W H H B H H W H B H M W B H M M H",	// 24 208 T 5056 7-Spliced (Score=7014518, COM=131, LR=3, ATW=343, music=193/50/41/13/1, LWM=85)  YEYSO-BSSCO- YC-BYSEC- BY-SCS LS-L YS-BBE LC-B EES- CYC-OC-S-B YYECY- BOB-ELL LOB-EEL- CCCSSSY- BBBOC-SEYY-O- OBLEY-OB ELS- BOOL-CLSCEC- OBB-BEL OL-OOS-CYS LC-O- YYLCS- EOB-OO- OS- LY-YCS- BEL-BBE- L-LOBO-E SS-EO EEY- E-OBEO L-SO- B(25/39) C(20/15) E(23/20) L(20/8) O(26/46) S(24/45) Y(20/20)  (LTLM=22, LA=30, homes=29) ATW
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
		"M W H M H W W B B H W H M M B B B H M H B H M H B W W B M M H M B M H M M B M M H B H B",	// 55 204 T 5024 7-Spliced (Score=7014538, COM=134, LR=3, ATW=343, music=199/58/37/10/4, LWM=80)  E-EY CE-B OY- L-BBOBE- YSCS-E CL-E YS-CE-BYCCO- BOLEE-O- BOC-CYYY CCS-OB-OOBL-BO LOB-BO B- BOS-YO- BYSCO-BO B- SLSSLS-LEELLL- SS-YS EY-B-E LOLES-EO E-OS-YO- L-OLLC-SC E-BY- YSY-OLEO E-SE OBOY-SLS-CCSLY-SCSS- YC-BBL- BCSEC-YEOEE B(23/21) C(19/15) E(24/41) L(20/16) O(26/32) S(25/48) Y(20/26)  (LTLM=15, LA=46, homes=27) ATW
		"M W H M H W W M H M M B M W B W B B W B H B B H M M H B H M H B H M W M H M B M H M M H",	// 56 207
		"M W H M H W W H W B M M H M B M H M M B M H B H M H B H M H B B H B W B H W H M M B M B",	// 57 204
		"M W H M H W W H W B M M H H M M H B W H M B H M H B H M H B H M M H M M H W H M M B M B",	// 58 215 T 5024 7-Spliced (Score=7014512, COM=127, LR=3, ATW=343, music=187/50/34/18/0, LWM=83)  L-OBBL-O- YEOEEY-OBBEL- YSYC-B-O- BEEEOB-E OO-SLOEL E-SE SYC-SO- LES- CYC-CCSC CSS-CYSS- CS-CLSY-YLO OS- E-YSSY CY-SC- BB-OBLEL- BLE-YS- BCCECC-LC- OBB-BYYCO- O-LY E-YYCY- BB-OBBBL L-LOBBL- BEOBO-CYS- E-OLOE O-OOS-EO L-SL YS-YC B(24/40) C(22/31) E(21/31) L(20/10) O(26/30) S(22/26) Y(22/19)  (LTLM=20, LA=54, homes=27) ATW
		"M W H M H W W H W B H H B W B M M H M M B M B B H M W M H M M B H M B B M W H M M B M B",	// 59 199
		"M W H M H W W H W B H H B W H H M H H M H B H M H B H M H B H M M H M M H W H M M B M B",	// 60 197
		"M W H M H W W H W W H B M H M M B M H M W M H M B M H M W M M H H B W B H W H M M B M B",	// 61 196
		"M W H M H W W H W W H M M B B M W W M H B H M W M H H M H B W B M W H H B W B H W H M W",	// 62 200
		"M W H M H W W H W W H M M W W M H B H M B M H M W M H M B M W H H B W B H W H M M B M B",	// 63 197
		"M W H M H H B B H M B B W W H W B B H W H M M W M M B M B B H M W M H M B M W H H B H B",	// 64 197
		"M W H M H H B M M B M H M M B M B M H M B B W W H W B B H W H M M H H M W M M H H B H B",	// 65 190
		"M W H M H H M W W M H B H M W M H H M H B W B M W H H B W B H W B W H W W H M M B M M W",	// 66 201
		"M W H M H H W M M W H W B W B M B H W B W W B H M M H B H M H B H M W M H M B M H M M H",	// 67 207 T 5088 7-Spliced (Score=7014495, COM=129, LR=3, ATW=343, music=184/43/38/13/3, LWM=83)  BSSLCC-LC CO-L- E-CL- OBOBO- YL-OEEC-YE YYS-EY CL-B B- LOLOB-CES-SS CL-YSO-BBB O-EY CY-BYSEC- SSCC-OOB-CS YL-CEO-E SS-EEO- SYS-CE L-OLLO OC- LCSLS-YC- YLCSEC-LEELEE- OBB-ESYEY- L-EC CO-B BB-OOBL- BELC-CE OBB-ESS-SE- O-YYSY YCS-EY- B(20/26) C(26/38) E(26/30) L(21/16) O(22/22) S(24/41) Y(20/11)  (LTLM=19, LA=46, homes=28) ATW
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
		"M W H B H M M B H H W W H W W H B M M H B W B M M W M M H M M B B W B H M H W W B",	// 214 T 5088 7-Spliced (Score=7014507, COM=126, LR=3, ATW=343, music=194/45/38/14/3, LWM=82)  CYC-Y-B CSCLS- LOLLY-BSSEC- L-OC-YCSL-ELO- LCO- BS-OOOE-B B- EEEOO-OELLEL-YYC- LOB-BO BLEC-EC YSS-YYSY- OBOY-BBB-YSE-OB O-CYESY-LOEE-E SYS-YE E-BBELEL- O-OOEE E-CSSY SY-BO BBO-CY YCYC-L OE-CC- L-OBOB OS- SL-SCS CSSY-SCC YS-CC B(20/12) C(24/22) E(23/44) L(19/12) O(26/36) S(24/44) Y(23/24)  (LTLM=18, LA=33, homes=26) ATW
		"M H M B M B B H W H B W B M M W M M B H B W W H W W M H B H M M B H B M M B H W B",	// 199
		"M H W B H M H B W W H W W M H B H M M B H W B M M H B W B M M W M M H M M B H W B"		// 209
	)

	val callings41b = List(
		"B B B H M W B M M H H B B M W B W W H W B M H H M H H M M B H B M B H M H B H M W",	// 203
		"B B B H M H B W W H W B M H H M H H M H B M M H H M M B H M H B H M M B B B H W B",	// 209	1
		"B B M H B M B H M H B H M H B H M M H H M H H W H M H B W W H B B M M H H B B M B",	// 201
		"B B M H W W B W W H W B B M H W H B H M M B H H B M M H H M H H W H M W B H M M H",	// 218	T 5088 7-Spliced (Score=7014506, COM=134, LR=2, ATW=343, music=186/45/41/12/1, LWM=86)  BYYLC-SO-EO BB-EOBLO- YO-E SE-OOB-YS EY-CSC LY-E- OBBOOC-YSY YS-BO EEO-BBE L-CYSS- CE-B EEC- EYCSO-BO B- BYSCES-B-YYSE-CEYYL- LCCSC- EOEES-SEC-LLOBL E-CO- ESO- YSY-OLLEL- OBOBL- CSCC-B OS- SCY-LOBL-E YY-CY- SEOLLS-CYSY L-OOLL- B(20/38) C(21/29) E(25/28) L(20/14) O(26/16) S(23/32) Y(24/29)  (LTLM=7, LA=34, homes=27) ATW
		"B B W H H M H B W H H W H M H B W W H W B B M H H M M B H M H B H M M B B B H W B",	// 195
		"B M H W H M H B W W H W B H M M H H M M B M M H H W M H B M B H M H B H M M B M B",	// 205
		"B H M H H M H H W H M H B W W H W B H H B M M H H W M H B M B H M H B H M M B M B",	// 207
		"M H W M H B B M H M M H M M H H W H M H B W W H W B B M M H B H M H B H M M B M B",	// 214	T 5088 7-Spliced (Score=7014499, COM=134, LR=3, ATW=343, music=184/48/34/12/1, LWM=92)  L-OBELO- LOBEE-OEES-OEOO- SS-LEE OE-SC E-BY- BCYECC-SL CSS-CL- L-OOBB L-OBEEL- CSCSYSC- LEOBL-E- SCC-YE- SS-SLOBOY-CEYSY-B OC- SSCS-B OBB-LO BLE-BO BB-CL O-CE- BBBLEC-SS- L-YSYS- YEOEL-BO B- CEOOY-CE O-YYYC YY-EO E-SYCS YS-YEOO B(22/50) C(21/6) E(27/20) L(18/13) O(27/42) S(25/42) Y(19/11)  (LTLM=30, LA=35, homes=28) ATW
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

	val mbd3part1 = "WMW BBH MMH  WM BBH MMH"*3
	val dfm = "BMH MMH MWHH BHH WBBH MMH "*3	// T 5088 7-Spliced (Score=7014524, COM=139, LR=2, ATW=343, music=190/57/41/17/0, LWM=92)  CY-LCS-YE- O-EES-YE- O-C-SCY- CYSCE- CLCYO-BEE- B- CL-SSO-SL-ECSEC- BLEY-B-YE- SS-OB O-EY- O-OC-YO- L-EELOB-B YSCLY- YSLSS- YC-YS- B- CL-SCO-SE-OO- BEEC-SL E-YO- OBOS-SES-EC- L-B-SO- O-C-B YSECS- SEE- LCYYE-BBL- OBOLO- BC-OEO-EC-OO- SCY-OBBO L-ES- B(18/6) C(24/10) E(27/27) L(16/9) O(27/92) S(27/32) Y(20/14)  (LTLM=28, LA=28, homes=29) ATW
	val dfm3part1 = "BMH MMH MWHH BHH WBBH MMH "
	val dfm3part2 = "BW BH MWHH BBWH BW BH MWHH BBW " // Don: T 5184 7-Spliced (Score=7009827, COM=138, LR=2, ATW=237, music=161/48/27/21/0, LWM=102)  YC-OOC-B LEL-BO B- L-Y-O- LSSYS- SC-YE-BE-E- YC-OOC-B LEL-BO B- L-Y-O- LSSYS- SC-YE-BE-E- B(24/12) C(18/10) E(24/41) L(24/12) O(24/26) S(24/43) Y(24/17)  (LTLM=16, LA=23, homes=12)
	val dfm3part3 = "MWHH BHH WBBH MMH MWHH BHH BBH MMH"	// Don: T 5184 6-Spliced (Score=6009648, COM=132, LR=3, ATW=233, music=195/66/33/24/3, LWM=100)  O-Y-E- B- SY-OB B- B- CE-SCO-YE-OO- CCY-ES E-SO- O-Y-E- B- SY-OB B- B- CESCO-YE-OO- CCY-ES E-SO- B(24/14) C(24/17) E(30/18) O(36/108) S(24/27) Y(24/11)  (LTLM=54, LA=54, homes=16)

	val betaSplit = "B H H W B B B B V'V' H H W B M W W"*3
	// T 5056 7-Spliced (Score=7014268, COM=133, LR=3, ATW=343, music=211/67/41/13/2, LWM=90)  OE-OE- LEC- BOBBL-E OL-SC YC-CE-LS-O'EOBBEO'SYCE- B- ES-B LLO-BSS-BC CO-B-E OBB-SY- SCCCYYY- SE-L YS-BC-SO-BBOOB-E'LYCS Y'CYCO- OC- YE-OOLES-CS E-BBLEO-OY-E YY-YS- OS- CL-OLE-BS-OE OO-BBL EOB-O'CYEY Y'LE- CEL- LY-SES-SC L-ELOL-CEYCC-E B(22/26) C(23/16) E(27/62) L(19/10) O(27/56) S(20/22) Y(20/19)  (LTLM=17, LA=30, homes=26) ATW
	// T 5088 7-Spliced (Score=7014263, COM=133, LR=3, ATW=343, music=212/64/42/14/0, LWM=91)  SY-YY- LCL- BOBLO-SEY-BC-SO-EC-O'EL LOEO'SYYO- EYYSC- OBBBO-E SY-SEOLL L-Y-OS-E YY-OB B- OC- OBOE-L CC-SE-CO-BEE EOB-E'LOBBLO'SCCE- LSL- CE-B CY-CLS-C-OS-E OBB-OE- ECCSY- CSCS-B BEO-EY-BO BEE-BBOOB-E'CSLY Y'EO- B- EY-L CS-BLL L-BS YL-B-E B(27/30) C(20/14) E(27/55) L(20/4) O(27/66) S(19/23) Y(19/20)  (LTLM=13, LA=41, homes=28) ATW


	//val calling = callings44(0)
	//val calling = betaSplit

	val calling = callings("agr3")
	//val calling = callings("new3")

	//val calling = callings("Seed 191")
	//val calling = callings("Bristol 5152 no.3")
	//val calling = "B M HMW M HB B MMB B B B B HB HB B HHW B B BW B M HB M HW BW HW HH"

	val methods = List(london, superlative, cambridge, yorkshire, cornwall, bristol, lessness)

	//override val seed = Some("PPE/core6_dfm.txt")
	//override val seed: Option[String] = Some("T 5024 7-Spliced (Score=7015150, COM=124, LR=3, ATW=343, music=241/68/46/14/3, LWM=80)  O-EEEOO- OBOBOC-SSY YYSC-YSE-SS O-OOBB-O- CY-OB BB-YCESS-OLOL OO-LEOEE-BBE EEO-SESC-B BEO-SC- LEEOEO- O-CO- B- O-OBBL-YYC YSY-S-E- LCO- BEOEO-E- YLCSLC-BY SL-E YSSY-L SS-CEOE-B-B LEE-LYSYL- CCSES- BLOEE-B E-EY- CSYC-O- B- LOOBE- B(22/38) C(16/10) E(31/44) L(15/5) O(32/92) S(23/40) Y(18/12)  (LTLM=42, LA=47, homes=27) ATW")

	//override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)


	def generate() = tunnel(this)
	//def generate() = prettyPrint()
	//def generate() = assessMethods()
	//def generate() = compareCallings()
	//def generate() = new TargetedSearch(this).varyFromSeed()

	//def generate() = new HelperCompositionSearch(this, helperPlan, "standard8/seed193.1.txt").varyFromSeed()
	//val helperPlan = new CompositionPlan(callings("Seed 193.1"), calls, getCallingMethod, musicDefs)

	//override def acceptNode(node: Node, leads: List[Lead]) = node.methods.forall(_==bristol) || node.music(0)>0


	val duffers = Set(Row("15324678"), Row("13567284"))
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		if (duffers(node.startLH))
			node.methodsUsed==Set(getCallingMethod) || leads.size<4
		else
			true
	}

	def scoreFn(comp: Composition) = atwScore(comp)

	def atwScore(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*35 -
				comp.falseScore*100 +
				ScoreFactory.balanceScore(comp)*1 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.longestNoComRun*5 + (if (comp.longestNoComRun<=3) 20 else 0) +
				4*comp.music(0)+4*comp.music(1)
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

	override lazy val musicDefs = Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens())
	//override lazy val musicDefs = Array(new MusicCompLib, new MusicLB(4), new Music56Rollup(), new Music65Rollup())

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