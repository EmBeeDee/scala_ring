package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{NamedMethod, PN, Row}

import scala.collection.mutable

/**
 * A method (or PN prefix) with the ability to generate the rows for a given lead.
 * @author mark
 */
trait SearchMethod
{
	val name: String
	val lhGroup: String
	val leadLength: Int
	def offsetTo2ndHalflead(firstHLpos: Int): Int
	val pn: List[PN]
	def generateLead(start: Row, builder: mutable.Buffer[Row]): Unit
	def isAllowedWith(other: SearchMethod): Boolean
	val nextLevelMethods: List[SearchMethod]
	override def toString = name+" ("+lhGroup+")"

}

case class FullSearchMethod(method: NamedMethod) extends SearchMethod
{
	val name = method.namePlusClass
	val lhGroup = method.lhGroup
	val leadLength = method.leadLength
	val pn = method.lead.toList

	val nextLevelMethods = Nil

	/** Prevents one method being used twice in the same composition */
	def isAllowedWith(other: SearchMethod) = other match
	{
		case fsm: FullSearchMethod => method!=fsm.method
		case _ => true
	}

	def generateLead(start: Row, builder: mutable.Buffer[Row])
	{
		method.generateLead(start, builder)
	}

	/** Given position in first halflead, return the position of the matching row in the second halflead */
	override def offsetTo2ndHalflead(firstHLpos: Int) = leadLength-firstHLpos-1

	override def equals(obj: scala.Any) = obj match
	{
		case that: SearchMethod => name == that.name
		case _ => false
	}
}

/** Methods must all have the same first section (up to "size" PN) and must all be the same LH group */
case class FirstSectionSearchMethod(size: Int, methods: List[NamedMethod]) extends SearchMethod
{
	val pn = methods.head.lead.slice(0,size).toList
	val name = PN.output(pn)
	val lhGroup = methods.head.lhGroup
	val leadEndPerm = methods.head.firstLeadEnd.toPerm
	val leadLength = (size+1)*2
	/** Allows the same section to be used more than once in the comp */
	def isAllowedWith(other: SearchMethod) = true

	lazy val nextLevelMethods = methods.map{FullSearchMethod(_)}

	def generateLead(start: Row, builder: mutable.Buffer[Row])
	{
		// Add the end row, too. Start row is also needed to ensure we don't have internal falseness with the composition leadheads
		builder+= PN.generateChanges(start, pn, builder)
		// Add the second half-lead
		builder+= PN.generateChanges(start.apply(leadEndPerm), pn, builder)
	}

	/** The second half lead is generated in reverse order! */
	override def offsetTo2ndHalflead(firstHLpos: Int) = leadLength/2+firstHLpos

	override def equals(obj: scala.Any) = obj match
	{
		case that: SearchMethod => name==that.name
		case _ => false
	}
}
