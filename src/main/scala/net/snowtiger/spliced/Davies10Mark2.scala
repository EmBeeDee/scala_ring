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
	val methods = List(lessness, cornwall, deva, bristol)
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