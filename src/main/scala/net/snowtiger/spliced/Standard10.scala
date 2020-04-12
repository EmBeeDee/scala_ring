package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition.{Composition, Major}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object Standard10 extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val superlative = NamedMethod("Superlative", 8, "-36-14-58-36-14-58-36-78", "12") 		// b, E
	val yorkshire = NamedMethod("Yorkshire", 8, "-38-14-58-16-12-38-14-78", "12")					// b, B
	val glasgow = NamedMethod("Glasgow", 8, "36-56.14.58-58.36-14-38.16-16.38","18")			// g, BE
	val london = NamedMethod("London", 8, "38-38.14-12-38.14-14.58.16-16.58", "12" )		// f, BD
	val belfast = NamedMethod("F Belfast", 8, "34-58.14-12-38.12-14.38.16-12.38", "18")		// m, BDE
	val cambridge = NamedMethod("Cambridge", 8, "-38-14-1258-36-14-58-16-78", "12")
	val rutland = NamedMethod("Rutland", 8, "x38x14x58x16x14x38x34x18", "12")
	val pudsey = NamedMethod("Pudsey", 8, "x58x16x12x38x14x58x16x78", "12")
	val lincolnshire = NamedMethod("Lincolnshire", 8, "x38x14x58x16x14x58x36x78", "12")


	// DFM 5120
	val calling = "MMWWH MBBWWH B MH' WH'"

	override lazy val calls = List(Major.Bob, Major.Single)

	//val methods = List(glasgow, belfast, london, rutland, superlative, pudsey, lincolnshire, cambridge, yorkshire, bristol)
	val methods = List(london, superlative, cambridge, yorkshire, bristol)
	//override val seed = Some("standard10\\BCDL.txt")

	def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)

	def generate() = tunnel(this)
}