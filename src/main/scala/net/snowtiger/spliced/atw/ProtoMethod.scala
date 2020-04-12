package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{Perm, Row}
import net.snowtiger.spliced.composition.Stage

/**
 * @author mark
 */

/** An abbreviation (A, B, C etc) and a leadhead perm */
case class ProtoMethod(abbrev: Char, perm: Perm)
{
	def applyTo(row: Row) = row.apply(perm)

	/** Go to lower-case abbreviation, signifying a second method of the same LH order */
	def bumpAbbrev =
	{
		abbrev.toLower
	}

	/** Go to numeric abbreviation, signifying a third method of the same LH order */
	def bumpAbbrev2 =
	{
		((abbrev-'A')+'1').toChar
	}

	/** Noddy implementation - A=1 ... F=6. See ProtoMethod.lhGroup() for better. */
	val lhOrder = ProtoMethod.lhOrder(abbrev)

	lazy val lhGroup = ProtoMethod.lhGroup(abbrev, perm.size)

	def nbells = perm.size

	override def toString = abbrev.toString
}

object ProtoMethod
{
	def apply(abbrev: Char, nbells: Int) =
	{
		val plainCourseLeads = new Stage(nbells).StageLeadheads.map{_.toPerm}.toArray
		new ProtoMethod(abbrev, plainCourseLeads(lhOrder(abbrev)))
	}

	def lhOrder(abbrev: Char) = (abbrev.toLower-'a')+1

	def lhGroup(abbrev: Char, nbells: Int) =
	{
		val order = abbrev.toLower-'a'
		val halfway = (nbells-2)/2
		val res = if (order<3)
			('a'+order).toChar.toString
		else if (order<halfway)
			"c"+(order-2)
		else if (order>=nbells-5)
			('d'+(order-nbells+5)).toChar.toString
		else
			"d"+(nbells-order-5)
		res
	}

}

