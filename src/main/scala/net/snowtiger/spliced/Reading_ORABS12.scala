package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * ORABS for Alan Reading
 *
 * @author mark
 */
object Reading_ORABS12 extends SplicedGenerator with SearchDefinitionBase
{
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val strathclyde = NamedMethod("Strathclyde", 12, "36x56.14.5Tx5T.36x14x3T.16x16.3T.16x16.3T.16x16.3T", "1T")	// k
	val rigel = NamedMethod("Rigel", 12, "36x7T.18x9T.50.36.14x1470.5T.14.36.9T.10.58x16.7T.16.70.16.ET", "1T")	// l
	val orion = NamedMethod("Orion", 12, "36-7T.18-9T.50.36.14-1470.5T.16-9T.30.18-14.3T.50.14-1T", "1T")			// mx
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")	// j2

	val callings = Array("WWH WWWH MMH MWMH WMWH",
		"WWHH MMH MW MH WWHH WWW MH")

	//val calling = "H MWW MH WH MH"							// RAP
	//val calling = "H MWH MH' M'W MWHH MH"							// MBD1
	//val calling = "MH W MWH"							// MBD1
	val compNum = 0
	println("COMP "+compNum)

	val methods = List(orion, strathclyde, rigel, avon, bristol)

	val calling = callings(compNum)
	//override val seed = Some("AlanReadingORABS/ORABS.txt")

	def generate() = tunnel(this)

	override lazy val musicDefs = Array(new MusicLB(4), new MusicLB(5), new MusicLB(6), new Music56Rollup(), new Music65Rollup())


	def scoreFn(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*20 +
				ScoreFactory.balanceScore2(comp)*4 +
				ScoreFactory.strictLenScore(comp, 0.2) -
				comp.longestNoComRun*8 + comp.com +
				comp.music(0)*2+comp.music(1)+comp.music(2)+comp.music(3)*2+comp.music(4)

	val shortNodes = Map(Row("12357496E8T0")->1, Row("1435267890ET")->2, Row("14327596E8T0")->1, Row("13247596E8T0")->1,
		Row("163548207T9E")->1, Row("1564237890ET")->1)

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		if (inSeed)
			true
		else
			node.size <= shortNodes.getOrElse(node.startLH, 11)
	}
}
