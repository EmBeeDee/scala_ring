package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Splice, _}

/**
 * Initial annealing set-up for the 2018 peal, but quite soon abandoned in favour of specialised brute-force
 * search for exact 9-parts - see {@link net.snowtiger.special.spliced.SpliceLHHLWeaver} for the splice-finding part,
 * and {@link net.snowtiger.special.spliced.CyclicSpliceSearch} for the method-populating bit.
 *
 * @author mark
 */
object OUSRoyal3 extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val precambrian = NamedMethod("Precambrian", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "12")		// f
	val pangaea = NamedMethod("Pangaea", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "10")						// l
	val snowTiger = NamedMethod("Snow Tiger", 10, "30-50.14-50-30.12-90.18-16-70.16-18.90", "12")				// a
	val sgurr = NamedMethod("G Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val small = NamedMethod("F Small's Friends", 10, "x50x14.50x12.30x3478x58x16x70.16x18.90", "10")			// k1
	val remus = NamedMethod("Remus", 10, "-3-4-2.5.6-34-5-6-67-6-7", "10")														// l
	val phobos = NamedMethod("O Phobos", 10, "-3-4-2.5.6-34-5-6-67-6.34.7", "12")
	val pooh = NamedMethod("H Pooh", 10, "3-5.4-5-36-34.7.8-6-7.6-78.10", "12")													// c1
	val eagleNebula = NamedMethod("Eagle Nebula", 10, "34x3.6x27.58.6x78x5.6x6.7.6x6.7", "10")					// l

	val moray = NamedMethod("Moray Firth", 10, "-50-16-1270-18-14-30-16-70-58-90", "12")								// b
	val independence = NamedMethod("Independence Day", 10, "30-50.14-50-30-34-50.16-16.70-16-70", "12")	// f
	val pollock = NamedMethod("K Pollockshields", 10, "36x56.14.50x50.16x14x50.14x14.30x12x10", "10")			// m
	val proteus = NamedMethod("U Proteus", 10, "34x50.14x50x30x14x50.14x14.70.16x18.90", "12")					// c1
	val galatea = NamedMethod("Galatea", 10, "34x50.16x56x10x14x50.14x14.70.16x18.90", "10")						// k1
	val copperdragon = NamedMethod("Copper Dragon", 10, "x5x4.5x5.36x4x58.4x4.7.6x78.1", "10")					// g
	val goldfinger = NamedMethod("Goldfinger", 10, "3x5.4x5x36.2x7.58.6x6.7x6x1", "12")									// b
	val harrison = NamedMethod("Harrison", 10, "x30x14x12.50.16x34x50x16x70.16x56.70", "12")						// f
	val ingoldsby = NamedMethod("i Ingoldsby", 10, "x30x14x56x36.14x34.50.16x16.70x16x70", "10")				// k1
	val ireby = NamedMethod("I Ireby", 10, "x34x4x2x3.2x4.5.6x6.7x6x7", "10")														// k1
	val johnby = NamedMethod("Johnby", 10, "x50x14.50x12.30x12x18x16x70.12x16.30", "10")								// j1
	val newJ1 = NamedMethod("j New J1", 10, "-50-14.50-12.30-12-10-16-70.1258.34-10", "10")							// j1
	val knifesmithgate = NamedMethod("Knifesmithsgate", 10, "x5x4.5x5.8.7x4.3.6x6.7x6x1", "10")					// m
	val lochdubh = NamedMethod("Lochdubh", 10, "x3x4x2.5.6x34.7.58.6x6.7x6x1", "10")										// m
	val nobottle = NamedMethod("Nobottle", 10, "3x5.4x2x36.7x4.58.6x6.7x6x9", "12")											// d1
	val raspberry = NamedMethod("Raspberry Crumble", 10, "30x50.14x50x30x12x18x56.14.30.12x18.90", "12") // f
	val ringingchat = NamedMethod("Ringing Chat", 10, "x50x14.50x12.36.70.34x10x16x70.16x18.90", "10")	// l
	val stanage = NamedMethod("s Stanage Edge", 10, "34x30.14x12x30x14x50.16x16.70x16x70", "10")				// l
	val stmary = NamedMethod("St Mary Abbots", 10, "x5x4.5x2.3x2x1x6x5x6x5", "12")											// b
	val usselby = NamedMethod("Usselby", 10, "34x5.4x2x3x2x5.6x6.7x6x7", "10")													// l
	val xapuri = NamedMethod("Xapuri", 10, "34x50.14x12x30.14x70.18.36x16.70x16x10", "10")							// l
	val zamara = NamedMethod("Zamara", 10, "x50x14.50x12.36x12.70.58.16x16.70x16x10", "10")							// m

	//val methods = List(bristol, sgurr, snowTiger, precambrian)
	//val methods = List(bristol, sgurr, triton, snowTiger, precambrian)
	//val methods = List(bristol, triton, sgurr, snowTiger)
	//val methods = List(bristol, triton, sgurr, phobos, snowTiger)
	//val methods = List(bristol, zamara, sgurr)
	//val methods = List(snowTiger, yorkshire, precambrian)
	val methods = List(nobottle, snowTiger, triton, ireby, johnby)

	val calling1 = "FFFx MIIx XBBx IVVx BSXx HXMx WMHx SHWx VWSx"
	val calling2 = "FFMI MIXB XBIV IVBS BSFF HXHX WMWM SHSH VWVW"
	val calling3 = "FFMIBp"
	val calling = calling3

	//override val seed = Some("ousRoyal3/BTGS.txt")
	def generate() = greedy(this)
	//override val seed = Some("ousRoyal3/BTGS.txt")
	//def generate() = prettyPrint()

	def scoreFn(comp: Composition) = ScoreFactory.musicFinder2(comp)

	override lazy val calls = List(Royal.Bob, Royal.Single, Royal.BigBob)
	val compositeMusicDef = new CompositeMusic(new MusicRun(4), new MusicRun(5), new MusicRun(6), new MusicAscendingRunTenorBehind(4,2), new MusicAscendingRunTenorBehind(5,2), new Music4Course(), new Music56Rollup(), new Music65Rollup(), new MusicTittumsRow())
	override lazy val musicDefs = Array(compositeMusicDef, new Music56Rollup(), new MusicTittumsRow(), new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music4Course())

	override def acceptSplice(splice: Splice) =
	{
		//splice.com==splice.methods.size-1 || (splice.com==0 && splice.firstMethod==getCallingMethod)

		//splice.methods.size<=4 || splice.methodsUsed.size>2 || (splice.com==0 && splice.firstMethod==getCallingMethod)

		//splice.com==0  || (splice.methods.size>4 && splice.com==1)

		//splice.com<3
		true
	}

	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		//node.isGood || node.splice.isInSingleMethodComp(method1))
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		true
		/*
		if (inSeed)
			true
		else
			node.internalCom<3
			//goodNode(node)
		*/
	}

	private def goodNode(node: Node) =
	{
		if (node.leadsWithoutMusic>0)
			false
		else if (node.startLH==Row("1908574632"))
		{
			val containsTittums = node.music(2)>0
			if (containsTittums)
				println("Node allowed: "+node)
			containsTittums
		}
		else
		{
			//true
			node.internalCom<3
		}
	}
}