package net.snowtiger.doublehelix

import net.snowtiger.ringing.{Method, PN, Row}

import scala.collection.mutable

/**
 * @author Mark
 */

object HelixoidSearcher
{
	val HalfLeadLength = 56
	val AllPN = Set(PN("x"), PN("14"), PN("58"), PN("18"))

	var c = 0

	def main(args: Array[String])
	{
		val rounds = Row("XXX.....")
		val startPN = PN("x")
		val row = rounds.apply(startPN)
		search(row, List(startPN), Set(rounds, row))
		println("nodes: "+c)
	}

	def search(row: Row, pnSoFar: List[PN], rowsUsed: Set[Row])
	{
		c+= 1
		if (pnSoFar.size==HalfLeadLength-1)
		{
			println(new Method(8, pnSoFar.reverse))
			return
		}
		val rowsSeenThisTime = mutable.Set[Row]()
		for (pn <- AllPN.filter{isRightPlace(_, pnSoFar.head)})
		{
			val newRow = row.apply(pn)
			if (!rowsSeenThisTime.contains(newRow) &&
					!rowsUsed.contains(newRow))
			{
				rowsSeenThisTime+= newRow
				search(newRow, pn::pnSoFar,
					rowsUsed+newRow)
			}
		}
	}
	def isRightPlace(thisPN: PN, prevPN: PN): Boolean =
		thisPN.isCross != prevPN.isCross

}
