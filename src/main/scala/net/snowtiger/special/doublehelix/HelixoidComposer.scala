package net.snowtiger.doublehelix

import net.snowtiger.ringing.{Perm, Row}


/**
 * @author Mark
 */

object HelixoidComposer
{
	val plain = new Perm("23174865")
	val bob = new Perm("23174586")
	//val bob2 = new Perm("23148765")
	val bob2 = new Perm("12374586")
	//val bob2 = new Perm("12374586")
	//val bob2 = new Perm("12348765")

	var longest = 1

	def main(args: Array[String])
	{
		search(Row(8), Set(), Nil, 0, Set())
	}

	/**
	 * The used set should already contain row, except for the case of rounds.
	 */
	def search(row: Row, used: Set[Row], touch: List[String], nbobs: Int, callingPositions: Set[Int])
	{
		if (row.isRounds && touch.size>=longest)
		{
			println(touch.size+" "+touch.reverse.mkString)
			longest = touch.size
		}
		else
		{
			val plainRow = row.apply(plain)
			if (!used.contains(plainRow))
				search(plainRow, used+plainRow, "P"::touch, nbobs, callingPositions)

			val callingPos = touch.size % 15
			if (callingPositions.contains(callingPos) || callingPositions.size<3)
			{
				val bobbedRow = row.apply(bob)
				if (!used.contains(bobbedRow))
				//if (plainRow.placeOf(8)==bobbedRow.placeOf(8))
					search(bobbedRow, used+bobbedRow, "-"::touch, nbobs, callingPositions+callingPos)
			}
			if (nbobs<4)
			{
				val extremeRow = row.apply(bob2)
				if (!used.contains(extremeRow))
					//if (plainRow.placeOf(8)==extremeRow.placeOf(8))
						search(extremeRow, used+extremeRow, "x"::touch, nbobs+1, callingPositions)
			}
		}
	}

}