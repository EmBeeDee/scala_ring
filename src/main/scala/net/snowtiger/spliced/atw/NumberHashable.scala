package net.snowtiger.spliced.atw

import net.snowtiger.spliced.tables.Numberable

/**
 * @author mark
 */

trait NumberHashable extends Numberable
{
	def canEqual(obj: Any): Boolean

	override def hashCode() = if (n<0) super.hashCode() else n

	override def equals(obj: scala.Any) =
	{
		if (n<0)
			super.equals(obj)
		else
			obj match
			{
				case that: NumberHashable => that.canEqual(this) && n==that.n
				case _ => false
			}
	}
}