package net.snowtiger

import net.snowtiger.ringing.Row

/**
 * @author Mark
 */

object Test
{
	val rounds = Row(9)

	def main(args: Array[String])
	{
		val all = Row.generateAll(9)
		val target = Row("684927153")
		println( all.filter{thirdLead(_)==target} )
		val target2 = target.apply(target.toPerm)
		println( all.filter{thirdLead(_)==target2} )
	}

	def thirdLead(firstLead: Row) =
	{
		val perm = firstLead.toPerm
		firstLead.apply(perm).apply(perm)
	}

}