package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod

/**
 * @author mark
 */

trait NiceMethods
{
	// Some PPE "try alsos"
	val kenninghall = NamedMethod("Kenninghall", 8, "-56-14-56-38.14-14.58.14-14.58", "18")	// l
	val jovium = NamedMethod("Jovium", 8, "-38-14-12.58.16-34-58-16-58", "12")							// d
	val bolonium = NamedMethod("U Bolonium", 8, "-38-14-12.58.16-34-58-16-58", "18")					// j
	val mareham = NamedMethod("Mareham", 8, "-58-14.58-12.38-12-18.36.12-18", "18")					// k

	// Used in both Davies10 and RobSpecial
	val bristol = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val cornwall = NamedMethod("Cornwall", 8, "-56-14-56-38-14-58-14-58", "18")						// l, cps
	val venusium = NamedMethod("Venusium", 8, "-56-14.56-58.36-14-58-36-18", "18")				// j, G

	// Used in GriffithsMajor
	val cooktownOrchid = NamedMethod("O Cooktown Orchid", 8, "-38-14-1256-18-12-58-16-78", "12")	// a
	val turramurra = NamedMethod("Turramurra", 8, "-38-14-1258-16-14-38-56-78", "12")			// b
	val burnopfield = NamedMethod("F Burnopfield", 8, "-38-14-1256-16-34-18-56-78", "12")	// d
	val longLawford = NamedMethod("w Long Lawford", 8, "34-34.18-56-18-14-58-56-58", "12")// f
	val garnet = NamedMethod("Garnet", 8, "-38-14-1256-18-12-58-16-18", "12")							// b
	val curium = NamedMethod("U Curium", 8, "-5-4-56-36-2-3-4-7", "18") 									// j
	val osbaldwick = NamedMethod("Osbaldwick", 8, "-3-4-2-36.4-34.5-6-5", "12")						// b

	// Used in Davies10 not Robspecial
	val deva = NamedMethod("Deva", 8, "-58-14.58-58.36-14-58-36-18", "18")								// j, G
	val lessness = NamedMethod("Lessness", 8, "-38-14-56-16-12-58-14-58", "12") 					// f, B
	val malpas = NamedMethod("Malpas", 8, "34-58.14-58-36-14-58.14-14.7", "18")						// l, D
	val superlative = NamedMethod("Superlative", 8, "-36-14-58-36-14-58-36-78", "12") 		// b, E
	val yorkshire = NamedMethod("Yorkshire", 8, "-38-14-58-16-12-38-14-78", "12")					// b, B
	val adelaide = NamedMethod("Adelaide", 8, "-58-14-56-36-14-58.36-36.18", "12")				// c, BE
	val glasgow = NamedMethod("Glasgow", 8, "36-56.14.58-58.36-14-38.16-16.38","18")			// g, BE

	// Used in RobSpecial
	val queenCamel = NamedMethod("Queen Camel", 8, "-58-14.58-56.38-14-58-14-18", "18")		// g, cps
	val bouchavesnes = NamedMethod("b Bouchavesnes", 8, "-58-14.58-12.36.12-14.58-14-18", "18")	// k, B

	val mytholm = NamedMethod("Mytholm", 8, "-58-14-12.58.16-14-58-14-18","12")						// e, BT
	val diamond = NamedMethod("Diamond", 8, "58.34-14-58-38-14-58-16-78", "12")						// d, DG
	val lutetium = NamedMethod("l Lutetium", 8, "38-58.14-12-36-14-58-36-18", "12")				// f, BG
	val windley = NamedMethod("Windley", 8, "38-58.14-58-38-34-58.16-34.78", "12")				// f, B
	val vicarage = NamedMethod("v Vicarage", 8, "58-58.14-58-36.14-14.58.14.36-18", "12")	// d, GO
	val shoesmith = NamedMethod("s Shoesmith Hall", 8, "x38x14x12.58.16.34x14.58x14x18", "18")	// k, BD
	val speedball = NamedMethod("Speedball", 8, "x38x14x12x36.14x12.58x56x18", "12")			// e, BDK

	val rook = NamedMethod("Rook and Gaskill", 8, "38-58.14-58-36-12-58-16-58", "12")			// f, B
	val lowerBeeding = NamedMethod("O Lower Beeding", 8, "-58-14-12-36-12-38-14-18", "18")	// h, B
	val henley = NamedMethod("Henley", 8, "58-58.14-58-38-14-58-16-58", "12")							// c, cps
	val bingley = NamedMethod("I Bingley", 8, "x34x14x58x36x34x38x14x58", "12")						// f, D
	val undercliffe = NamedMethod("Undercliffe", 8, "36x56.14.58x1256.38x14x58x14x18", "18")	// j, BE
	val tavus = NamedMethod("Tavus", 8, "x58x1458x58x38x14x1458.36x14.58", "18")					// m, E

	val tovil = NamedMethod("t Tovil", 8, "36x56.14.58x58.16x14x1458x14x18", "18")				// g, BE
	val ephebe = NamedMethod("Ephebe", 8, "58x58.14x58x36.14x14.58x56x18", "12")					// b, D
	val grossular = NamedMethod("Grossular", 8, "x58x14x1256x16x14x58x16x78", "18")				// j, FG
	val kalium = NamedMethod("Kalium", 8, "x58x14.58x56.38x14x58x14.36.78", "18")					// h, cps
	val orton = NamedMethod("Orton", 8, "x58x14.58x12.38x34x58.14x56.18", "18")						// k, B
	val melrose = NamedMethod("Melrose Abbey", 8, "38x56.14x1256x38.14x14.58.14x36.78", "12") // f, BE
	val venonae = NamedMethod("v Venonae", 8, "36x56.14.58x12.38x14x58x12.36.18", "18")		// m, BE
	val olicaria = NamedMethod("o Olicaria", 8, "34x58.14x12x16x14x1458x14x18", "18")			// m, BD
	val netheravon = NamedMethod("Netheravon", 8, "34x58.14x58x38x14x58.16x16.78", "18")	// j, DG
	val radstone = NamedMethod("r Radstone", 8, "34x58.14x58x36.14x14.58.36.14x58", "18")	// m, DEK
	val sagarmatha = NamedMethod("Sagarmatha", 8, "36x56.14.58x12.38x12x38x14x78", "12")	// f, BE
	val upsilon = NamedMethod("Upsilon", 8, "34x58.14x58x36.14x12.58x1256x18", "18")			// m, BD
	val victoria = NamedMethod("v Victoria Bar", 8, "38x58.14x58x38.12x12.38x14x78", "12")	// f, B

	val onnum = NamedMethod("o Onnum", 8, "x38x14x12x36.12x14.38.14x14.58", "12")						// f, BD
	val bromford = NamedMethod("$ Bromford", 8, "x38x14x56x16x34x58x56x18", "12")					// e, B
	val burley = NamedMethod("u Burley Park", 8, "34x58.14x58x36.14x34.1258x12.36.18", "18")	// m, BD
	val derby = NamedMethod("d Derby", 8, "-38-14-58-16-12-38-56-18", "12")								// c, BD
	val frome = NamedMethod("Frome", 8, "x38x14x58x16x12x38x56x18", "18")									// h, BD
	val sulloniacae = NamedMethod("Sulloniacae", 8, "34x58.14x58x36x14x1458x14.56.18", "18")	// m, D
	val hanwell = NamedMethod("h Hanwell", 8, "38x58.14.58x58.36x12x38x14x78", "12")				// f, B

	val metheringham = NamedMethod("m Metheringham", 8, "-58-14-12-16-34-38-14-58", "12")	// f, BD
	val foxton = NamedMethod("Foxton", 8, "x58x14.58x12.38.12x14.58.36x14.58", "12")			// e, BE
	val gold = NamedMethod("Gold", 8, "x58x14x56x36.14x14.58.14.36x78", "12")							// d, BE
	val chiltern = NamedMethod("c Chiltern", 8, "-58-14-12-36-12-38-14-78", "12")					// d, B
	//val macclesfield = NamedMethod("m Macclesfield", 8, "-58-14-56-36-14-58.36-36.18", "18")	// h, BE

	val micklegate = NamedMethod("Micklegate Bar", 8, "-58-14.58-12.36.12-14.58-14-18", "12")		// e, B
	val jamesCollege = NamedMethod("James College", 8, "-58-14.58-12.36.12-14.38.12-16.78", "12")	// f, B
	val harston = NamedMethod("Harston", 8, "-58-14.58-12.38.12-14.58.36-14.58", "18")		// k, BE
	val flerovium = NamedMethod("Flerovium", 8, "-58-14.58-12.38-12-58-14.36.78", "12")		// d, BE
	val davies = NamedMethod("Davies", 8, "38-58.14-58-38-34-58.16-56.78", "12")					// f, BD

	val cadmium = NamedMethod("K Cadmium", 8, "36-56.14.58-58.16-12-38.14-56.18", "18")		// j, BE

	// Search methods not used
	val vendredi = NamedMethod("v Vendredi Treize", 8, "-56-14-56-38-14-58.14-56.18", "18")	// k, B
	val eleuthera = NamedMethod("Eleuthera", 8, "-58-14.58-56.38.14-34.1258-16-18", "18") // j, B
	val cancer = NamedMethod("@ Cancer", 8, "34-58.14-58-36-14-58-14-18", "18")						// k, D
	val selwood = NamedMethod("s Selwood", 8, "34-58.14-58-36-14-58-14-18", "18")					// e, D
	val stohelit = NamedMethod("H Sto Helit", 8, "34-58.14-58-36-14-58.16-16.38", "18")		// g, DG
	val morganite = NamedMethod("m Morganite", 8, "-38-14-56-16-12-58-12.36.18", "12")		// e, B
	val aurum = NamedMethod("a Aurum", 8, "-38-14-56-16-12-58.36.12-18", "12")						// e, B
	val foulness = NamedMethod("Foulness", 8, "-38-14-56-16-12-58-14-58", "18") 					// l, B
	val dunster = NamedMethod("d Dunster", 8, "-58-14.58-58.36-14-58-36-18", "12")				// d, G
	val yateley = NamedMethod("y Yateley", 8, "58-58.14-58-36-14-58-36-18", "12")					// d, G

	// Cornwall alternatives (Cornwall below-work) - none any real use for Rob Special (not even Falmouth)
	val tellurium = NamedMethod("Tellurium", 8, "38-56.14-56-1238-14-58-14-58","12")			// a, B
	val barbican = NamedMethod("b Barbican", 8, "38-58.14-58-1238-14-58-14-58", "12")			// a, B
	val falmouth = NamedMethod("f Falmouth", 8, "-56-14-56-38-14-58-14-58", "12")						// f, cps

	val catseye = NamedMethod("c Cat's Eye", 8, "-58-14.58-12.38-12-58.14-14.58", "18")		// l, B
	val heptonstall = NamedMethod("Heptonstall", 8, "-58-14.58-58.36-14-58-34-78", "12")	// a, B
	val ashtead = NamedMethod("a Ashtead", 8, "-58-16-56-36-34-38-14-78", "12")							// d, cps
	val essex = NamedMethod("e Essex", 8, "x58x14.58x58.16x14x38.14x14.58", "18")					// h, cps
	val northampton = NamedMethod("Northampton", 8, "x34.58.14x58x38.14x14.58x14x58", "12")	// f, D
	val xenon = NamedMethod("Xenon", 8, "-58-14.58-56.38.14-14.38.14-34.18", "12")				// d, cps
	val radon = NamedMethod("Radon", 8, "-58-14.58-56.38.14-14.38.14-34.18", "18")				// j, cps
	val xebec = NamedMethod("x Xebec", 8, "x58x14.58x56.38.14x14.58.1236x56.78", "18")			// h, cps

	val ely = NamedMethod("$ Ely", 8, "-38-14-56-16.34-14.58.14-14.58", "12")							// f, B

	val lancashire = NamedMethod("l Lancashire", 8, "58-58.14-58-36-14-58.14-14.78", "12")// a, cps
	val sonning = NamedMethod("s Sonning", 8, "38-38.16-56-36.14-12.38.16-56.38", "12")		// b, BDI
	val vanadium = NamedMethod("v Vanadium", 8, "x58x16x12x16x14x1258x12x18", "12")					// c, B

	//val jersey = NamedMethod("Jersey", 8, "-58-16-12-16-14-58-36-58","12")								// b, BN
	//val january = NamedMethod("January", 8, "56-56.14.56-56.38-14-1458-12-58", "12")			// f, cps
	val jevington = NamedMethod("jevington", 8, "-58-14.58-58.36.14-14.58.36.12.36.18", "18")	// j, cps
	val london = NamedMethod("| London", 8, "38-38.14-12-38.14-14.58.16-16.58", "12" )		// f, BD
	val ytterbium = NamedMethod("t Ytterbium", 8, "-38-14-1256-16-12-58.16-12.78", "12")	// d, B
	val woodstock = NamedMethod("Woodstock", 8, "-38-14-58-16-12-38-14-78", "18")					// g, B
	//val cassiobury = NamedMethod("K Cassiobury", 8, "-58-16-12-36-12-58-14-18", "12")			// c, B
	val belfast = NamedMethod("f Belfast", 8, "34-58.14-12-38.12-14.38.16-12.38", "18")		// m, BDE
	val cambridge = NamedMethod("@ Cambridge", 8, "-38-14-1258-36-14-58-16-78","12")


}