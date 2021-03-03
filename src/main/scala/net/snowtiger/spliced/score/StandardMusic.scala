package net.snowtiger.spliced.score

import net.snowtiger.ringing.{CompositeMusic, Music, Row}
import net.snowtiger.spliced.StandardMethods

object StandardMusic extends StandardMethods
{
	def main(args: Array[String]): Unit =
	{
		val m = new MusicCompLib
		val rows = lessness.generateFullCourse(Row(8))
		println(rows.map(m.countMusic).sum)
	}
}

/**
 * @author mark
 */
class StandardMajorMusic extends CompositeMusic(new MusicRun(4), new Music65Rollup(), new MusicQueens())

class StandardMaxMusic extends CompositeMusic(new MusicLB(4), new MusicLB(5), new Music56Rollup(), new Music65Rollup())

class CoursingMaxMusic extends CompositeMusic(new MusicLB(4), new MusicLB(5), new Music56Rollup(), new Music4Course(), new Music65Rollup())


class Music4Course extends Music
{
	val tittumsUp = List(0,2,4,6)
	val tittumsDown = tittumsUp.reverse
	val pbUp = List(0,6,2,4)
	val pbDown = pbUp.reverse
	
	def countMusic(row: Row) = 
	{
		val n = row.nbells
		val back4 = List(row.placeOf(n-3), row.placeOf(n-2), row.placeOf(n-1), row.placeOf(n))
		val lowestPlace = back4.min
		val back4Shifted = back4.map{_ - lowestPlace}
		if (back4Shifted==tittumsUp || back4Shifted==tittumsDown || back4Shifted==pbUp || back4Shifted==pbDown)
			1
		else
			0
	}
}

/**
 * Also works with higher stages.  Counts runs 5-n front and back, in both directions.
 */
class Music56Rollup extends Music
{
	def countMusic(row: Row) =
	{
		val n = row.nbells
		var m = 0
		if (row.bellAt(5)==5 && ascendingRun(row, 5, n))
			m+= 1
		else if (row.bellAt(5)==n && descendingRun(row, 5, n))
			m+= 1
		else if (row.bellAt(1)==5 && ascendingRun(row, 1, n-4))
			m+= 1
		else if (row.bellAt(1)==n && descendingRun(row, 1, n-4))
			m+= 1
		m
	}

}

/**
 * Also works with higher stages.  Counts runs 657-n off the back ascending only.
 */
class Music65Rollup extends Music
{
	def countMusic(row: Row) =
	{
		val n = row.nbells
		var m = 0
		if (row.bellAt(5)==6 && row.bellAt(6)==5 && row.bellAt(7)==7 && ascendingRun(row, 7, n))
			m+= 1
		m
	}
}

/** Counts CRUs at the back. Works on Major and higher even stages. */
class MusicCRU extends Music
{
	val combinationBells = Set(4,5,6)

	def countMusic(row: Row) =
	{
		val n = row.nbells
		var m = 0
		if (combinationBells(row.bellAt(5)) && combinationBells(row.bellAt(6)) && row.bellAt(7)==7 && ascendingRun(row, 7, n))
			m+= 1
		m
	}
}

/** Counts forward CRUs at the front, a la CompLib. Works on Major and higher even stages. */
class MusicCRUFront extends Music
{
	val combinationBells = Set(4,5,6)

	def countMusic(row: Row) =
	{
		val n = row.nbells
		var m = 0
		if (combinationBells(row.bellAt(1)) && combinationBells(row.bellAt(2)) && row.bellAt(3)==7 && ascendingRun(row, 3, n-4))
			m+= 1
		m
	}
}

/** Counts combinations of the back four bells, front or back */
class MusicBack4Combinations extends Music
{
	def countMusic(row: Row) =
	{
		val n = row.nbells
		val minBack = n-3
		var m = 0
		if (row.bellAt(1)>=minBack && row.bellAt(2)>=minBack && row.bellAt(3)>=minBack && row.bellAt(4)>=minBack)
			m+= 1
		else if (row.bellAt(n)>=minBack && row.bellAt(n-1)>=minBack && row.bellAt(n-2)>=minBack && row.bellAt(n-3)>=minBack)
			m+= 1
		m
	}
}

/** Counts back-4 and "named change" combinations scored by CompLib. Only works on 8 bells! */
class MusicCompLibMajorCombinations extends Music
{
	val r5678 = Row("5678")
	val r8765 = Row("8765")
	val r6578 = Row("6578")
	val r7568 = Row("7568")
	val r7658 = Row("7658")
	val r5768 = Row("5768")
	val r7468 = Row("7468")
	val r3478 = Row("3478")
	val r3578 = Row("3578")
	val r3468 = Row("3468")
	val r2468 = Row("2468")

	def countMusic(row: Row) =
	{
		val n = row.nbells
		assert(n==8, "MusicCompLibMajorCombinations only works on 8")
		var m = 0
		val frontFour = row.extract(1,4)
		val backFour = row.extract(5,8)
		// Note many are double-counted by CompLib
		m+= 2*scoreMatch(backFour, r7568)
		m+= 2*scoreMatch(backFour, r5768)
		m+= scoreMatch(backFour, r5678)
		m+= scoreMatch(backFour, r8765)
		m+= scoreMatch(backFour, r7658)
		m+= scoreMatch(backFour, r7468)
		m+= scoreMatch(backFour, r3478)
		m+= scoreMatch(backFour, r3578)
		m+= scoreMatch(backFour, r3468)
		m+= scoreMatch(backFour, r2468)
		m+= 2*scoreMatch(frontFour, r6578)
		m+= scoreMatch(frontFour, r5678)
		m+= scoreMatch(frontFour, r8765)
		m+= scoreMatch(frontFour, r7568)
		m+= scoreMatch(frontFour, r7658)
		m+= scoreMatch(frontFour, r5768)
		if (row.bellAt(2)==5 && row.bellAt(4)==6 && row.bellAt(6)==7 && row.bellAt(8)==8)
			m+= 1
		m
	}
}

class MusicNearMiss extends Music
{
	override def countMusic(row: Row) =
	{
		val rounds = Row(row.nbells)
		val matches = rounds.bells.zip(row.bells).map((p)=> p._1==p._2)
		val nonMatches = matches.dropWhile(_==true).reverse.dropWhile(_==true)
		if (nonMatches.size==2)
			1
		else
			0
	}
}

/** Counts runs of the given length, either ascending or descending and front or back */
class MusicRun(runLength: Int) extends Music
{
	def countMusic(row: Row) =
	{
		val n = row.nbells
		var m = 0
		// run at front counts 1
		if (ascendingRun(row, 1, runLength) || descendingRun(row, 1, runLength))
			m+= 1
		// run at back counts 1
		if (runLength<n)
			if (ascendingRun(row, n-(runLength-1), n) || descendingRun(row, n-(runLength-1), n))
				m+= 1
		m
	}
}

/** Counts all runs, from minRunLength up to nbells, adds them all together */
class MusicMultiRun(val minRunLength: Int) extends Music
{
	val runCounters = (minRunLength to 20).map(new MusicRun(_)).toList

	override def countMusic(row: Row) =
		runCounters.take(row.nbells-minRunLength+1).map(_.countMusic(row)).sum
}

/** Tries to replicate CompLib music scores, including internal runs of n-4 bells or more,
	* plus CRUs and back bell combinations on eight bells. Doesn't include many named changes. */
class MusicCompLib extends Music
{
	val common = new CompositeMusic(new MusicMultiRun(4), new Music56Rollup, new Music65Rollup, new MusicNearMiss,
		new MusicQueensRow, new MusicTittumsRow, new MusicKingsRow)
	val major = new CompositeMusic(new MusicCRU, new MusicCRUFront, new MusicCompLibMajorCombinations)

	override def countMusic(row: Row) = common.countMusic(row) + countStageSpecific(row)

	def countStageSpecific(row: Row): Int =
	{
		val nbells = row.nbells
		val multiRunInternal = new MusicMultiRun(nbells-4)
		var score = countInternalMusic(row, multiRunInternal)
		if (nbells==8)
			score+= major.countMusic(row)
		score
	}

	protected def countInternalMusic(row: Row, runCounter: MusicMultiRun): Int =
	{
		if (row.nbells-2>=runCounter.minRunLength)
		{
			val internalRow = row.extract(2,row.nbells-1)
			runCounter.countMusic(internalRow) + countInternalMusic(internalRow, runCounter)
		}
		else
			0
	}

}

/** Counts runs including those split by a cyclic row such as (on eight) 43287651. Ignores the treble (but not completely...). */
class MusicCyclicRun(runLength: Int) extends MusicRun(runLength)
{
	override protected def isSucceedingBell(row: Row, placeOfSmaller: Int, placeOfBigger: Int) =
		super.isSucceedingBell(row, placeOfSmaller, placeOfBigger) ||
				(row.bellAt(placeOfSmaller)==row.nbells && row.bellAt(placeOfBigger)==2)
}

class MusicAscendingRunTenorBehind(runLength: Int, finishingBell: Int) extends Music
{
	def countMusic(row: Row) =
	{
		val n = row.nbells
		if (row.bellAt(n)!=n || row.bellAt(n-1)!=finishingBell)
			0
		else if (descendingRun(row, n-runLength, n-1))
			1
		else
			0
	}
}

class MusicLB(runLength: Int, maxBell: Int) extends Music
{
	def this(runLength: Int) = this(runLength, runLength+3)

	def countMusic(row: Row) =
	{
		val n = row.nbells
		var m = 0
		// LB run at front counts 1
		if ( (row.bellAt(runLength)<=maxBell && ascendingRun(row, 1, runLength)) || (row.bellAt(1)<=maxBell && descendingRun(row, 1, runLength)))
			m+= 1
		// LB run at back counts 1
		val backRunStart = n-(runLength-1)
		if ( (row.bellAt(n)<=maxBell && ascendingRun(row, backRunStart, n)) || (row.bellAt(backRunStart)<=maxBell && descendingRun(row, backRunStart, n)))
			m+= 1
		m
	}
}

/**
 * Counts rows ending 246 on five and six; 2468 or 3468 on seven and eight; 4680 on nine and ten bells;
 * 680T on eleven and twelve bells, etc.
 */
class MusicQueens extends Music
{
	def countMusic(row: Row) =
	{
		val oddStage = row.nbells%2==1
		val n = row.nbells + (if (oddStage) 1 else 0)
		val r = if (oddStage) Row(row.toString + Row.Rounds.substring(n, n)) else row
		val b1 = r.bellAt(n-3)
		val b2 = r.bellAt(n-2)
		val b3 = r.bellAt(n-1)
		val b4 = if (oddStage) n else r.bellAt(n)
		var good = b4==n && b3==n-2 && b2==n-4
		if (n==8)
			good = good && (b1==n-6 || b1==n-5)
		else if (n>8)
			good = good && b1==n-6
		if (good) 1 else 0
	}
}

/** Count actual queens row */
class MusicQueensRow extends Music
{
	def countMusic(row: Row) = scoreMatch(row, SpecialRows.queens(row.nbells))
}

class MusicWhittingtonsRow extends Music
{
	val whittingtons8 = Row("12753468")

	override def countMusic(row: Row) =
	{
		assert(row.nbells == 8, "Whittingtons only defined on 8")
		scoreMatch(row, whittingtons8)
	}
}

/** Count actual kings row */
class MusicKingsRow extends Music
{
	def countMusic(row: Row) = scoreMatch(row, SpecialRows.kings(row.nbells))
}

class MusicTittumsRow extends Music
{
	def countMusic(row: Row) = scoreMatch(row, SpecialRows.tittums(row.nbells))
}

/** Counts +1 for rows which are handstroke or backstroke leads of Plain Bob at the same stage. */
class MusicPlainCourseLeads extends Music
{
	override def countMusic(row: Row) =
	{
		if (row.bellAt(1)==1 && row.isPlainBob)
			1
		else
			0
	}
}

object SpecialRows
{
	def queens(n: Int) =
	{
		var queens: List[Int] = Nil
		for (i <- n to 1 by -2)
			queens = i :: queens
		for (i <- n-1 to 1 by -2)
			queens = i :: queens
		Row(queens)
	}

	def kings(n: Int) =
	{
		var kings: List[Int] = Nil
		for (i <- n to 1 by -2)
			kings = i :: kings
		for (i <- 1+n%1 to n by 2)
			kings = i :: kings
		Row(kings)
	}

	def tittums(n: Int) =
	{
		val halfN = (n+1)/2
		var revTittums: List[Int] = Nil
		if (n%1==1)
			revTittums = halfN::revTittums
		for (i <- 1 to n/2)
			revTittums = (i+halfN)::i::revTittums
		val tittums = Row(revTittums.reverse)
		tittums
	}
}
