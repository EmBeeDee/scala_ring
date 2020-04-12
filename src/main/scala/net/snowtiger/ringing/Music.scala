package net.snowtiger.ringing

/**
 * @author mark
 */

trait Music
{
	def countMusic(row: Row): Int

	def countMusic(rows: Iterable[Row]): Int = rows.map(countMusic).sum

	protected def isSucceedingBell(row: Row, placeOfSmaller: Int, placeOfBigger: Int) =
		row.bellAt(placeOfSmaller)+1 == row.bellAt(placeOfBigger)

	protected def ascendingRun(row: Row, from: Int, to: Int): Boolean =
	{
		if (to<=from)
			true
		else
			isSucceedingBell(row, from, from+1) && ascendingRun(row, from+1, to)
	}

	protected def descendingRun(row: Row, from: Int, to: Int): Boolean =
	{
		if (to<=from)
			true
		else
			isSucceedingBell(row, from+1, from) && descendingRun(row, from+1, to)
	}

}

class CompositeMusic(music: Music*) extends Music
{
	def countMusic(row: Row) = music.map(_.countMusic(row)).sum
}