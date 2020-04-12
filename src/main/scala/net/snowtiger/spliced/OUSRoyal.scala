package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Splice, _}

/**
 * @author mark
 */

object OUSRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val precambrian = NamedMethod("Precambrian", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "12")		// f
	val pangaea = NamedMethod("Pangaea", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "10")		// l
	val sgurr = NamedMethod("Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val small = NamedMethod("Small's Friends", 10, "x50x14.50x12.30x3478x58x16x70.16x18.90", "10")			// k1

	//val methods = List(yorkshire, sgurr, triton, bristol, precambrian)
	//val methods = List(triton, bristol, yorkshire, precambrian)
	//val methods = List(sgurr, bristol, yorkshire, precambrian)
	//val methods = List(small, bristol, yorkshire, triton)
	val methods = List(bristol, yorkshire, pangaea, triton)
	//val methods = List(sgurr, bristol, yorkshire, triton)

	val callingTT = "H MWH MH' M'W MWH H MH"
	val calling1 = "WH BxW WH H W'H' BM MH' M' FV Hx Hx Wx Wx Sx VW"
	val calling2 = "WH BxW WH H W'H' BM MH' M' FV Hx Hx Wx Wx Sx Sx"
	val calling3 = "WH BxW WHx H H BXH XM M XMx FV Hx Hx Wx Wx Sx VW"					// 5760
	val calling4 = "WH BxW WH H WBxH MWH BXH XM M XMx FV Hx Hx Wx Wx Sx VW"		// 6440
	val calling5 = "MWH H W BxW H BXH XM M XMx FV Hx Hx Wx Wx Sx VW"        	// 5400
	val calling6 = "WH Bx WH WBx WH BXH XM M XMx FV Hx Hx Wx Wx Sx VW"				// 5360
	val calling = calling4

	override val seed = Some("ousRoyal/calling4_lowCOM.txt")
	def generate() = tunnel(this)
	//override val seed = Some("ousRoyal/pretty.txt")
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