package net.snowtiger.spliced.composition

import net.snowtiger.ringing.PN

/**
 * @author mark
 */

case class Call(postfix: Option[Char], symbol:Char, splicedSymbol: String, pn: PN)
{
	/** Constructor for plains */
	def this(pn: PN) = this(None, ' ', "", pn)

	def isPlain = postfix.isEmpty

	/* Returns a pair (postfix char, this). Only works for calls - error for plain */
	def me = postfix.get->this

	override def toString = if (postfix.isEmpty) "" else postfix.get.toString
}

object Call
{
	def apply(postfix: Char, symbol:Char, splicedSymbol: String, pn: PN):Call = Call(Some(postfix), symbol, splicedSymbol, pn)
}

