package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod

/**
 * @author mark
 */

trait StandardMethods
{
	val bristol = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val superlative = NamedMethod("Superlative", 8, "-36-14-58-36-14-58-36-78", "12") 		// b, E
	val yorkshire = NamedMethod("Yorkshire", 8, "-38-14-58-16-12-38-14-78", "12")					// b, B
	val cambridge = NamedMethod("Cambridge", 8, "-38-14-1258-36-14-58-16-78", "12")			// b
	val lincolnshire = NamedMethod("N Lincolnshire", 8, "x38x14x58x16x14x58x36x78", "12")	// b
	val pudsey = NamedMethod("Pudsey", 8, "x58x16x12x38x14x58x16x78", "12")							// b
	val london = NamedMethod("London", 8, "38-38.14-12-38.14-14.58.16-16.58", "12" )			// f, BD

	val lessness = NamedMethod("E Lessness", 8, "-38-14-56-16-12-58-14-58", "12") 				// f, B
	val cornwall = NamedMethod("O Cornwall", 8, "-56-14-56-38-14-58-14-58", "18")						// l, cps
	val glasgow = NamedMethod("Glasgow", 8, "36-56.14.58-58.36-14-38.16-16.38","18")			// g, BE
	val cassiobury = NamedMethod("K Cassiobury", 8, "-58-16-12-36-12-58-14-18", "12")			// c, B

}