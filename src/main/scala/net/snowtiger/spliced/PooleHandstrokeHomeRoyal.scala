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

object PooleHandstrokeHomeRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val precambrian = NamedMethod("Precambrian", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "12")		// f
	val pangaea = NamedMethod("Pangaea", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "10")						// l
	val sgurr = NamedMethod("Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val eagleNebula = NamedMethod("Eagle Nebula", 10, "34x3.6x27.58.6x78x5.6x6.7.6x6.7", "10")					// l
	val small = NamedMethod("Small's Friends", 10, "x50x14.50x12.30x3478x58x16x70.16x18.90", "10")			// k1
	val remus = NamedMethod("H Remus", 10, "-3-4-2.5.6-34-5-6-67-6.34.7", "10")												// l

	//val methods = List(bristol, yorkshire, pangaea, triton)
	//val methods = List(precambrian, bristol, yorkshire, triton)
	//val methods = List(sgurr, bristol, yorkshire, pangaea)
	//val methods = List(sgurr, bristol, triton, precambrian)
	//val methods = List(sgurr, bristol, triton, eagleNebula)
	//val methods = List(sgurr, bristol, precambrian, eagleNebula)
	val methods = List(remus, bristol, sgurr, precambrian)
	//val methods = List(triton, bristol, phobos, sgurr)

	val secondPlaceFinish = false
	val bellBobbedThroughFirst = 10

	val calling = (if (bellBobbedThroughFirst==2) "MI MI XB XB IV IV BS BS FF " else "FF MI MI XB XB IV IV BS BS ") +
								(if (secondPlaceFinish) "Wh" else "Hh")

	//def generate() = tunnel(this)
	override val seed = Some("PooleHandstrokeHome/pretty8.txt")
	def generate() = prettyPrint()

	def scoreFn(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*2 +
				8*(ScoreFactory.balanceScore(comp)) +
				lengthScore(comp)*40 +
				(ScoreFactory.strictLenScore(comp)*3)/2 -
				comp.com*3 -
				comp.longestNoComRun*2 +
				(if (comp.isAtw) 100 else 0) +
				comp.music(0)+10*comp.music(1)

	def lengthScore(comp: Composition) =
	{
		val length = comp.length
		if (length<5001)
			(length-5100)/2
		else if (length>5180)
			(5200-length)/2
		else
			0
	}


	val compositeMusicDef = new CompositeMusic(new Music56Rollup(), new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music4Course(), new Music65Rollup())
	override lazy val musicDefs = Array(compositeMusicDef, new MusicPlainCourseLeads(), new Music56Rollup(), new MusicRun(4), new MusicRun(5), new MusicRun(6), new Music4Course())

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
		if (inSeed)
			true
		// For handstroke home finishing at the Wrong, we need a second's place method as the last one.
		else if (node.startLH.coursingOrder(getCallingMethod.plainPerm)=="864235790")
		{
			if (secondPlaceFinish)
				node.methods.last.is2ndsPlace
			else
				node.methods.last.isNthsPlace
		}
		else
			true
			//node.internalCom<4
			//goodNode(node)
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
			node.internalCom<3
		}
	}
}