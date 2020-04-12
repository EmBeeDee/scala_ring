package net.snowtiger.spliced

import net.snowtiger.ringing.{Method, PN}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

import scala.io.Source

/**
 * @author mark
 */

object RobSpecial extends SplicedGenerator with SearchDefinitionBase with NiceMethods
{
	// "The" 5056
	//val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MW MW MWW MH"
	// The 5120 version
	val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MMBWW MW MWW MH"

	//var methods = List(speedball, vicarage, windley, mytholm, bouchavesnes, queenCamel, venusium, cornwall, bristol)
	//val methods = List(tavus, henley, lowerBeeding, rook, bouchavesnes, queenCamel, venusium, cornwall, bristol)
	//val methods = List(davies, ephebe, rook, tovil, shoesmith, venusium, cornwall, bristol)

	//val methods = List(victoria, orton, kalium, ephebe, rook, tovil, shoesmith, venusium, cornwall, bristol)
	//val methods = List(windley, orton, kalium, ephebe, rook, tovil, shoesmith, venusium, cornwall, bristol)
	val methods = List(kalium, rook, tovil, shoesmith, cornwall, bristol)
	//override val seed = Some("robspecial3\\vOKERTSVCB.txt")
	//override val seed = Some("robspecial3\\KRTSCB.txt")

	def diffStart(m: Method): Boolean = {val pn =m.lead; pn.head!=PN("58") && !pn.startsWith(List(PN("x"), PN("58")))}

	val methodAssessor = MethodAssessor(8)

	val extraMethods = methodAssessor.parseMethods(Source.fromFile("surprise.csv")).
			filter(methodAssessor.isGood).
			filter(methodAssessor.hasNewWork(methods)).
			filter(diffStart) //.filter(MethodAssessor.hasNewLHGroup(methods)).filter(MethodAssessor.hasNewStart(methods))

	def scoreFn(comp: Composition) = ScoreFactory.musicFinder(comp)

	def generate() = new MultiMethodGenerator(this, extraMethods).multi(100000)

}