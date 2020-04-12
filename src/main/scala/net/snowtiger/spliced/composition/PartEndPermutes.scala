package net.snowtiger.spliced.composition

import net.snowtiger.ringing.Row

/**
 * @author mark
 */

trait PartEndPermutes
{
	def nparts: Int
	def permuteByPartEnds(leadhead: Row): List[Row]

}

object OnePart extends PartEndPermutes
{
	override def nparts = 1
	override def permuteByPartEnds(leadhead: Row) = List(leadhead)
}
