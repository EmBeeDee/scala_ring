package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod, Row}
import net.snowtiger.spliced.GunningMax.Music7890ETOffFront
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * @author mark
 */

object HallMax extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val cambridge = NamedMethod("Cambridge", 12, "x3x4x25x36x47x58x69x70x8x9x0xE", "12")											// b
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")	// j2
	val yorkshire = NamedMethod("Yorkshire", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18xET", "12")						// b
	val phobos = NamedMethod("Phobos", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "1T") // l

	val calling1 = "H WMMH M WWM WWMHHH WHH WWMH"
	val calling2 = "H W MMMWH WW MH"
	val calling2a = "H W MMMWH WW MH H W MMMWH WW MH"

	val calling3 = "H WWH W MW MWW MHH MWW MWHH WWH MM"
	val calling4 = "H W MMW MWH MMHHH MH W MH WW'h"
	val calling5 = "H W MMW MWH MMHHH MH W MWH"
	val calling6 = "H WHH MMH WMH WMH WWH WWHH"

	//val methods = List(bristol, phobos, yorkshire)
	//val methods = List(bristol, zanussi, yorkshire)
	val methods = List(zanussi, cambridge, yorkshire, bristol)

	val calling = calling6
	//override val seed = Some("maxHall/hallseed.txt")
	//override val seed = Some("maxHall/hall2.txt")
	//override val seed = Some("maxHall/2part1hall.txt")

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	//def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)
	def scoreFn(comp: Composition) = sf(comp)

	def sf(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				//ScoreFactory.balanceScore(comp)*2 +
				ScoreFactory.strictLenScore(comp) -
				skewedBalance(comp)*10 -
				15*comp.longestNoComRun -
				30*comp.falseScore +
				25*(comp.atwScore) +
				comp.music(0)*2 + comp.music(1)*30 + comp.music(2)*2 -
				1*(comp.longestAbsence)-1*comp.leadsToLastMethod -
				10*comp.leadsWithoutMusic

	def skewedBalance(comp: Composition): Int =
	{
		val methods = comp.methodsUsed.toList
		val counts = comp.methodCounts(methods).zip(methods).map{(p)=> if (p._2==yorkshire || p._2==cambridge) p._1*2 else p._1}
		val min = counts.reduceLeft(math.min)
		val max = counts.reduceLeft(math.max)
		max - min
	}

	val compositeMusicDef = new CompositeMusic(new MusicLB(4), new MusicLB(5), new Music56Rollup(), new Music65Rollup(), new Music7890ETOffFront())
	//override lazy val musicDefs = Array(compositeMusicDef, new MusicRun(12), new Music7890ETOffFront(), new Music56Rollup(), new MusicLB(4), new MusicLB(5), new Music65Rollup())
	override lazy val musicDefs = Array(compositeMusicDef, new MusicRun(12), new Music56Rollup(), new MusicLB(4), new MusicLB(5), new Music65Rollup())


	/** Leads in the node passed as a convenience, to avoid cost of regen */
	/*
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val singleMethodNodes = Set("156348207T9E", "14627395E8T0", "1563427890ET").map{Row(_)}
		!singleMethodNodes(node.startLH) || node.methodsUsed.size==1
	}
	*/

	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		node.startLH!=Row(12) || node.methodsUsed.contains(bristol) || node.methodsUsed.contains(zanussi)
	}
}
