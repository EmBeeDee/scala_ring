package net.snowtiger.spliced

import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object Davies10Mark2 extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	// "The" 5056
	val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MW MW MWW MH"
	// The 5120 version
	//val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MMBWW MW MWW MH"

	//val calling = "BWH WW B B B MHH BHH B B" // Alpha
	//val calling = "B H H W B B B B H H W B M W W"	// Beta
	//val calling = "M M H B B B B M H H B B M H W" // Gamma
	//val calling = "M M H WW B B B M H H B B M W W" // Gamma bis
	//val calling = "B W W B M B H W B B B B H B W" //Delta

	//var methods = List(speedball, vicarage, windley, mytholm, bouchavesnes, queenCamel, venusium, cornwall, bristol)
	//val methods = List(tavus, henley, lowerBeeding, rook, bouchavesnes, queenCamel, venusium, cornwall, bristol)
	val methods = List(venusium, glasgow, yorkshire, adelaide, superlative, malpas, lessness, cornwall, deva, bristol)
	// T 5024 10-Spliced (Score=10006027, COM=142, LR=3, ATW=490, music=205/59/41/19, LWM=93)  B- VBVDCG-B SY-BVBA- BVLDS-LC-GSDC GA-ACCG-AYGAL-MDGL-BM B- BYVGM-SVAG-YSLDLA- BDLVGM-CGC MS- CGY-CMD ALDC-SS GGYG-SYYY SV-BMBBL- SSY-LVL DBM-L SD-CBBM-VLVAM M-LVGSD-GAS VA-ASGA-CCMD-CGY YC-YDS-MLCBM L-CV VBVDC- A(13/5) B(18/64) C(18/24) D(14/16) G(18/17) L(16/23) M(14/10) S(17/30) V(16/12) Y(13/4)  (LTLM=36, LA=40, homes=21) ATW

	//override val seed = Some("onepart2\\5056-BCDL.txt")

	def scoreFn(comp: Composition) =
	{
		val sf = ScoreFactory(comp)
		sf.methodsUsed.size*1000000 -
				30*comp.falseScore +
				4*sf.balanceScore +
				10*sf.atwScore +
				(1*sf.music(0)+4*sf.music(1)+sf.music(2)) -
				5*sf.longestNoComRun -
				3*sf.longestAbsence +
				sf.strictLenScore
				//sf.atwBoost
	}

	def generate() = tunnel(this)

}