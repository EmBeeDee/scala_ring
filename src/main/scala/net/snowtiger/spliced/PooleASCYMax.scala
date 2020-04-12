package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * @author mark
 */

object PooleASCYMax extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val bristolHLS = NamedMethod("X Bristol HLS", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x90ET", "1T")				// j
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")	// j2
	val avon = NamedMethod("Avon", 12, "x5x4.5x5.30.4x70.1.36x9.30.8x8.9x8x1", "1T")
	val pixie = NamedMethod("Pixie", 12, "-12-10-129T-189T-12-16-129T-149T")
	val skip = NamedMethod("Skip", 12, "-129T.58.149T-1258-16-129T-189T-12-10")

	val calling = "MWH "

	val methods = List(bristol, zanussi, bristolHLS, avon)
	//val methods = List(bristol, zanussi, yorkshire)

	override val seed = Some("pooleASCY/seed.txt")

	def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)

	def musicFinder2(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				ScoreFactory.balanceScore(comp)*2 +
				ScoreFactory.strictLenScore(comp) +
				4*(comp.atwScore) +
				(1*comp.music(0)+4*comp.music(1)+40*comp.music(2)) -
				1*(comp.longestAbsence)-1*comp.leadsToLastMethod -
				10*comp.leadsWithoutMusic

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		val singleMethodNodes = Set("156348207T9E", "14627395E8T0", "1563427890ET").map{Row(_)}
		true || !singleMethodNodes(node.startLH) || node.methodsUsed.size==1
	}
}
