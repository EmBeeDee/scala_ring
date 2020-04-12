package net.snowtiger.spliced

import net.snowtiger.ringing.{Music, NamedMethod}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object TomGriffithsMajor extends SplicedGenerator with SearchDefinitionBase
{
	val calling1 = "H H BH BFIH FIBH VMMM VIBF VWIBF W BFIWFIH"
	val calling2 = "MBFIH IBFW BFIV IBFFBH' H' FWWW IH W BFIWWH BV MMMFBH"
	val calling2a = "MBFIH IBFW BFIV IBFFB FWWW IH W BFIWWH BV MMMFBH"
	val calling2b = "MBFIH IBFW BFIV IBFFB FWWW IH W BFIWHHH WH BV MMMFBH"
	val calling2c = "MBFIH IBFW BFIV IBFFB FWWW IH W BFIWH WWWHH WH BV MMMFBH"
	val calling2d = "MBFIH IBFW BFIV IBFFBHHH FWWW IH W BFIWWH BV MMMFBH"
	val calling2e = "MBFIH IBFW BFIV IBFFBHHH FWWW IH W BFIWHHH WH BV MMMFBH"
	val calling2f = "MBFIH IBFW BFIV IBFFBHHH FWWW IH W BFIWH WWWHH WH BV MMMFBH"
	val calling3 = "MHH V'F'B' V'W M'M'W MW"
	val calling = calling2e

	val bristol = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val cornwall = NamedMethod("Cornwall", 8, "-56-14-56-38-14-58-14-58", "18")						// l, cps
	val deva = NamedMethod("Deva", 8, "-58-14.58-58.36-14-58-36-18", "18")								// j, G
	val lessness = NamedMethod("Lessness", 8, "-38-14-56-16-12-58-14-58", "12") 					// f, B
	val superlative = NamedMethod("Superlative", 8, "-36-14-58-36-14-58-36-78", "12") 		// b, E
	val venusium = NamedMethod("Venusium", 8, "-56-14.56-58.36-14-58-36-18", "18")				// j, G
	val yorkshire = NamedMethod("Yorkshire", 8, "-38-14-58-16-12-38-14-78", "12")					// b, B

	val burnopfield = NamedMethod("F Burnopfield", 8, "-38-14-1256-16-34-18-56-78", "12")	// d
	val conistonBluebird = NamedMethod("Z Coniston Bluebird", 8, "-3-4-2-1-2-5.4-6.7", "12" )	// b
	val cooktownOrchid = NamedMethod("K Cooktown Orchid", 8, "-38-14-1256-18-12-58-16-78", "12")	// a
	val garnet = NamedMethod("Garnet", 8, "-38-14-1256-18-12-58-16-18", "12")							// b
	val longLawford = NamedMethod("W Long Lawford", 8, "34-34.18-56-18-14-58-56-58", "12")// f
	val osbaldwick = NamedMethod("Osbaldwick", 8, "-3-4-2-36.4-34.5-6-5", "12")						// b
	val turramurra = NamedMethod("Turramurra", 8, "-38-14-1258-16-14-38-56-78", "12")			// b
	//val lowerBeeding = NamedMethod("w Lower Beeding", 8, "-58-14-12-36-12-38-14-18", "18")// h, B
	//val curium = NamedMethod("U Curium", 8, "-5-4-56-36-2-3-4-7", "18") 									// j
	val unrung = NamedMethod("Unrung", 8, "-34-4-56-3-2-25-6-8", "18")										// j
	val tovil = NamedMethod("V Tovil", 8, "36-56.14.58-58.16-14-1458-14-18", "18")				// g
	val livermorium = NamedMethod("V Livermorium", 8, "38-58.14-12-36.14-14.38-12-58", "12")	// e
	val quidenham = NamedMethod("Quidenham", 8, "36-56.14.58.12.58.36.14-14.58-14-18", "18")	// g
	val xxxb = NamedMethod("XXXB", 8, "38-58.14-58-36.12-14.38-12-58", "12")							// c
	val yateley = NamedMethod("A Yateley", 8, "58-58.14-58-36-14-58-36-18", "12")					// d
	val cowdrayCastle = NamedMethod("D Cowdray Castle", 8, "58-34.16-56-18-14-1458.36-16.58", "12")	//d
	val ickenham = NamedMethod("Ickenham", 8, "58-58.14-58-36.14-14.38-34-58", "12")			// c
	val vicarage = NamedMethod("Vicarage", 8, "58-58.14-58-36.14-14.58.14.36-18", "12")		// d
	val henley = NamedMethod("Henley", 8, "58-58.14-58-38-14-58-16-58", "12")							// c
	val diamond = NamedMethod("Diamond", 8, "58.34-14-58-38-14-58-16-78", "12")						// d
	val waldron = NamedMethod("A Waldron", 8, "58.34-14-58-38.14-14.38.14-14.78", "12")		// d
	val stWistans = NamedMethod("St Wistan's", 8, "38-58.14-58-18-14-58.14.36-58", "12")	// c
	val zagarolo = NamedMethod("Zagarolo", 8, "58.34-1458-12-18-12-38.14-56.78", "12")		// d

	//val methods = List(cornwall, turramurra, yorkshire, lessness)
	//val methods = List(longLawford, bristol, cooktownOrchid, cornwall, turramurra, yorkshire, lessness)
	val methods = List(zagarolo, unrung, osbaldwick, longLawford, bristol, cooktownOrchid, cornwall, turramurra, yorkshire, lessness)

	override val seed = Some("tomGriffiths\\seedBCKLOTUWY_Zagarolo2e.txt")
	//override val seed = Some("tomGriffiths\\seedBCKLTWY2e.txt")
	//override val seed = Some("tomGriffiths\\seedCLTY2e.txt")

	//def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)
	def scoreFn(comp: Composition) = ScoreFactory.musicFinder(comp)

	//def generate() = anneal(this)
	def generate() = prettyPrint()
	//def generate() = new MultiMethodGenerator(this, extraMethods).multi(200000)

	override lazy val musicDefs: Array[Music] = Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens, new MusicTittumsRow)

	/*
	val methodAssessor = MethodAssessor(8)

	val extraMethods = methodAssessor.parseMethods(Source.fromFile("tdMajor.csv")).
			filter(methodAssessor.isVeryGood).filter(methodAssessor.hasNewWork(methods)).
			filter(methodAssessor.hasNewStart(methods)).filter(methodAssessor.hasNewLHGroup(methods))
	extraMethods.foreach
	{ (m) =>
		println(m.namePlusClass + " " + m.outputPN()+" = "+m.firstLeadHead+" ("+m.lhGroup+")")
	}
	*/

	/*
	override def acceptSplice(splice: Splice) =
	{
		val inSeed = splice.methodsUsed == Set(getCallingMethod)
		inSeed || splice.com>=splice.methods.size-2
	}
	*/
}