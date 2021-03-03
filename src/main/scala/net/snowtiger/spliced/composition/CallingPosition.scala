package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{Method, PN, Row}

import scala.collection.mutable

/**
 * @author mark
 */
case class CallingPosition(name: String, plainCourseLH: Row, stageDef: StageCalls)
{
	val maskedLH = stageDef.maskAllButTenor(plainCourseLH)
	lazy val tenorPos = plainCourseLH.placeOf(stageDef.nbells)

	override def toString = name

	/** Produces the LH resulting from applying call "call" at this calling position following the leadhead "lh" */
	def apply(lh: Row, call: PN) =
	{
		def doApply(current: Row): Row =
		{
			val bobbed = current.apply(stageDef.callPerm(call))
			if (positionReached(bobbed))
			{
				bobbed
			}
			else
			{
				val plained = current.apply(stageDef.plainPerm)
				assert(lh!=plained, "Couldn't find calling position "+this+" from "+lh)
				doApply(plained)
			}
		}
		doApply(lh)
	}

	def get(lh: Row) = stageDef.get(lh)
	def maskAllButTenor(r: Row) = stageDef.maskAllButTenor(r)

	def positionReached(lh: Row): Boolean = maskedLH==maskAllButTenor(lh)
	def positionReached(tenorPos: Int): Boolean = tenorPos==this.tenorPos
}

class Stage(val nbells: Int)
{
	val rounds = Row(nbells)
	private val PN1n = "1"+Row.Rounds(nbells-1)
	private val protoMethod = new Method(nbells, PN1n+".12")
	val StageLeadheads = protoMethod.leadHeads

	def maskAllButTenor(r: Row) = r.maskFrontBells(r.nbells-1)

	def callPerm(call: PN) = protoMethod.callPerm(call)
	def plainPerm = protoMethod.plainPerm

	val Plain12 = new Call(PN("12"))
	val Plain1N = new Call(PN(PN1n))
	val Bob = Call('-', '-', ".", PN("14"))
	val Single = Call('\'', 'S', "'", PN("1234"))
	val BigBob = Call('x', 'x', "x", if (nbells>10) PN("18") else PN("16"))
}

object Stage
{
	val cache = mutable.Map[Int,Stage]()
	def apply(nbells: Int) = cache.getOrElseUpdate(nbells, new Stage(nbells))
}

abstract class StageCalls(nbells: Int) extends Stage(nbells)
{
	val All: Set[CallingPosition]

	val Home = apply("H", 0)
	val Wrong = apply("W", -1)
	val Middle = apply("M", -2)
	val Before = apply("B", 3)

	lazy val ByCall = All.map{ (cp)=> (cp.name, cp) }.toMap
	lazy val ByMaskedLH = All.map{ (cp)=> (cp.maskedLH, cp) }.toMap

	def apply(name: String, tenorPos: Int): CallingPosition =
	{
		val tenor = Row.Rounds(nbells-1)
		val pos = if (tenorPos<=0) nbells+tenorPos else tenorPos
		val maskStr = Row.MaskChar.toString
		val maskedLH = Row( maskStr*(pos-1) + tenor + maskStr*(nbells-pos) )
		val plainCourseLH = StageLeadheads.filter{ maskAllButTenor(_)==maskedLH }
		assert(plainCourseLH.size==1, "Couldn't find a plain course LH to match calling position "+maskedLH)
		CallingPosition(name, plainCourseLH.head, this)
	}

	def get(lh: Row) =
	{
		var g = ByMaskedLH.get(maskAllButTenor(lh))
		assert(g.isDefined)
		g
	}

}

object Major extends StageCalls(8)
{
	val Fifths = apply("V", -3)
	val Fourths = apply("F", 4)
	val In = apply("I", 2)

	val All = Set(Home, Wrong, Middle, Before, Fifths, Fourths, In)
}

object Royal extends StageCalls(10)
{
	val Sevenths = apply("S", -3)
	val Fourths = apply("F", 4)
	val Fifths = apply("V", 5)
	val Sixths = apply("X", 6)
	val In = apply("I", 2)

	val All = Set(Home, Wrong, Middle, Before, In, Sevenths, Fourths, Fifths, Sixths)
}

object Maximus extends StageCalls(12)
{
	val Ninths = apply("N", -3)
	val In = apply("I", 2)
	val Fourths = apply("F", 4)

	val All = Set(Home, Wrong, Middle, In, Before, Fourths, Ninths)
}

object Sixteen extends StageCalls(16)
{
	override val All: Set[CallingPosition] = Set(Home, Wrong, Middle, Before)
}
