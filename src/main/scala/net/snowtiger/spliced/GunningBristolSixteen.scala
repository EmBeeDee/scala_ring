package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod, PN}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Splice, _}

/**
 * @author mark
 */

object GunningBristolSixteen extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 16, "-5D-14.5D-5D.36.14-7D.58.16-9D.70.18-ED.9T.10-AD.EB.1T-1T.AD-1T-1D", "1D")	// j4
	val littleport = NamedMethod("Littleport", 16, "-5D-14.5D-5D.36.14-14.5D-14-18", "1D")																// m
	val bristollittle = NamedMethod("X Bristol Little", 16, "-5D-14.5D-5D.36.14-7D.58.16-16.7D-16-10", "1D")							// g
	val maypole = NamedMethod("Maypole", 16, "-5D-14.5D-5D.36.147D.58.169D.70.18ED.9T.10AD.EB.1T-1T.AD-1T-1D", "1D")			// j4

	val methods = List(bristollittle, littleport, bristol)

	val calling1 = "Bz HsHx /168472503T9BEDAC Bz HsHx /142638507T9BEDAC Hp"
	val calling2 = "Hz HsHx /168472503T9BEDAC Hz HsHx /142638507T9BEDAC Hp"
	val calling = calling2

	override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	val single = Call('s', 's', "s", PN("1278"))
	val bigbob = Call('x', 'x', "x", PN("18"))
	//val bigsingle = Call('z', 'z', "z", PN("1TABCD"))
	val bigsingle = Call('z', 'z', "z", PN("123456"))
	override lazy val calls = List(Sixteen.Bob, single, bigbob, bigsingle)

	def scoreFn(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				lengthScore(comp)*40 -
				comp.leadsWithoutMusic*5 +
				comp.music(0)

	def lengthScore(comp: Composition) =
	{
		val length = comp.length
		val delta = -Math.abs(length - 4500)
		delta
	}

	val musRun5 = new MusicRun(5)
	val musRun6 = new MusicRun(6)
	val musRun7 = new MusicRun(7)
	val musRun8 = new MusicRun(8)
	//val mus4Course = new Music4Course()
	val compositeMusicDef = new CompositeMusic(musRun5, musRun6, musRun7, musRun8)
	override lazy val musicDefs = Array(compositeMusicDef, musRun6, musRun7, musRun8)

	override def acceptSplice(splice: Splice) =
	{
		true
	}

	private def goodNode(node: Node) =
	{
		true
	}
}