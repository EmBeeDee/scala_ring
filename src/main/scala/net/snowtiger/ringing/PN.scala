package net.snowtiger.ringing

import net.snowtiger.ringing.Row.{MaxStage, Rounds}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * @author Mark
 */
case class PN(pn: String)
{
	// These are zero-based
	private val unterminatedPlaces = parsePN(pn)
	// Add a final place on to avoid the place notation list running out during permute operations
	private val places = unterminatedPlaces ++ List(Int.MaxValue)

	def isCross = nPlaces == 0

	def nPlaces = places.size - 1

	def toPerm(nbells: Int) = Row(nbells).apply(this).toPerm

	def +(other: PN): PN =
	{
		def combine(xs: Seq[Int], ys: Seq[Int]): Seq[Int] =
		{
			if (ys.isEmpty)
				xs
			else if (xs.isEmpty)
				ys
			else
			{
				val x = xs.head
				val y = ys.head
				if (x == y)
					x +: combine(xs.tail, ys.tail)
				else if (x < y)
					x +: combine(xs.tail, ys)
				else
					y +: combine(xs, ys.tail)
			}
		}
		PN(combine(unterminatedPlaces, other.unterminatedPlaces))
	}

	def hasConsecutivePlacesWith(other: PN) = places.intersect(other.places).size > 1

	def acceptableConsecutive(other: PN) = this!=other && !hasConsecutivePlacesWith(other)

	def intersection(other: PN) = PN(places.intersect(other.places))

	def isRightPlaceAgainst(other: PN) = isCross || other.isCross

	def shiftUp(n: Int) = PN(unterminatedPlaces.map(_ + n))

	/** Shift up all places above the given place (which should be specified in 1-based notation, although we hold internally in zero-based) */
	def shiftUpAbove(place: Int, by: Int) = PN(unterminatedPlaces.map((x) => if (x + 1 > place) x + by else x))

	def shiftDown(n: Int) = PN(unterminatedPlaces.map(_ - n))

	/** Shift down all places below the given place (which should be specified in 1-based notation, although we hold internally in zero-based) */
	def shiftDownBelow(place: Int, by: Int) = PN(unterminatedPlaces.map((x) => if (x + 1 < place) x - by else x))

	def reverse(nBells: Int) = PN(unterminatedPlaces.reverse.map(nBells - 1 - _))

	def lowestPlace = if (isCross) None else Some(places.head)

	def highestPlace = if (isCross) None else Some(unterminatedPlaces.reverse.head)

	/** Returns true if either if the pns is the cross, of if the highest place of one is below the lowest of the other. */
	def disjointPlaces(other: PN) =
	{
		if (isCross || other.isCross)
			true
		else
			lowestPlace.get > other.highestPlace.get || highestPlace.get < other.lowestPlace.get
	}

	/** Return a list of any and all sub-PNs with consecutive places. E.g. 125670 -> 12, 567 */
	def consecutives: List[PN] =
	{
		val buf = ListBuffer[PN]()
		var cur = List[Int]()
		def addConsec()
		{
			if (cur.size > 1) buf += PN(cur.reverse)
		}
		for (place <- places)
			cur match
			{
				case x :: tail if x != place - 1 => addConsec(); cur = List(place)
				case _ => cur = place :: cur
			}
		addConsec()
		buf.reverse.toList
	}

	/** Returns true if the pair (lowPlace, lowPlace+1) are allowed to swap */
	def allowsSwap(lowPlace: Int) =
	{
		val p = lowPlace-1
		val placeAbove = places.find(_ >= p).get
		val placeBelow = places.reverse.find(_ < p).getOrElse(-1)
		placeAbove>lowPlace && (p-placeBelow)%2==1
	}

	/** Returns the number of bells crossing in odd places, i.e. PN 1450 gives 6 because of the three pairs (23) (67) (89) */
	def oddPairsCrossing(nBells: Int) =
	{
		var n = 0
		val placePairs = places.grouped(2)
		placePairs.filter(_.size==2).map((pair)=> pair.tail.head-pair.head-1).sum
	}

	def isPlace(place: Int) = places.contains(place - 1)

	def swapsSign(nBells: Int) =
	{
		val nPairsSwapped = (nBells - (places.size - 1)) / 2
		nPairsSwapped % 2 == 1
	}

	def placesAbove(pos: Int) = PN(places.filter
	{
		_ + 1 > pos
	})

	def placesBelow(pos: Int) = PN(places.filter
	{
		_ + 1 < pos
	})

	private def parsePN(pn: String): Seq[Int] = PN.concreteType(pn).flatMap(parsePnChar(_))

	/** Normal case-class equality on String pn seems flaky, but is conceptually wrong anyway. */
	override def equals(other: Any) = other match
	{
		case that: PN => places == that.places
		case _ => false
	}

	override def hashCode() = places.hashCode()

	override def toString =
	{
		def placeToString(p: Int) = if (p > MaxStage) "" else "" + Rounds(p)
		if (isCross) "-" else places.map(placeToString(_)).mkString
	}

	private def parsePnChar(c: Char): Option[Int] = Rounds.indexOf(c) match
	{
		case -1 => None
		case n => Some(n)
	}

	/**
	 * Permute the given row by the current PN.
	 * Note we do this by recursion on Lists of both bell characters and places - this appears to be the most efficient way,
	 * outperforming either using a buffer to accumulate the results, or permuting in place in an array.
	 *
	 * @param s
	 * @return
	 */
	def permute(s: Seq[Char]): Seq[Char] =
	{
		def perm1(pos: Int, s: List[Char], pn: List[Int]): List[Char] =
		{
			s match
			{
				case Seq() => s
				case Seq(a) => s
				case a :: b :: tail => perm2(pos, a, b, tail, pn.head, pn.tail)
			}
		}

		def perm2(pos: Int, a: Char, b: Char, tail: List[Char], nextPlace: Int, remainingPn: List[Int]): List[Char] =
		{
			if (pos < nextPlace - 1)
				b :: a :: perm1(pos + 2, tail, nextPlace +: remainingPn)
			else if (pos == nextPlace - 1)
				a :: b :: perm1(pos + 2, tail, remainingPn)
			else
				a :: perm1(pos + 1, b :: tail, remainingPn)
		}

		PN.concreteType(perm1(0, s.toList, places.toList))
	}

}

object PN
{
	def concreteType[T](xs: Seq[T]) = xs.toList

	def apply(placeList: Seq[Int]): PN =
	{
		val prunedList = placeList.reverse match
		{
			case x +: tail if (x == Int.MaxValue) => tail.reverse
			case _ => placeList
		}
		if (prunedList.exists(_ < 0))
			throw new Exception("Bad place list - negative value!")
		if (prunedList.exists(_ >= MaxStage))
			throw new Exception("Bad place list - negative value!")
		PN(prunedList.map(Rounds(_)).mkString)
	}

	/** Can cope with missing terminating places, i.e. 4 allowed for 14 */
	def apply(pn: String, nbells: Int): PN = PN(addTerminatingPlaces(pn, nbells))

	/** This one requires terminating places, i.e. 14 not 4. */
	def parse(s: String): Seq[PN] = normalise(s).map(PN(_))

	def parseToList(s: String): List[PN] = parse(s).toList

	/** Can cope with missing terminating places, i.e. 4 allowed for 14 */
	def parse(s: String, nbells: Int): Seq[PN] = normalise(s).map
	{
		PN(_, nbells)
	}

	def parseToList(s: String, nbells: Int): List[PN] = parse(s, nbells).toList

	private def normalise(s: String): Seq[String] =
	{
		val surround = s.trim().replace("-", "x").replace("x", ".x.")
		val trim = if (surround.startsWith(".")) surround.substring(1) else surround
		concreteType(trim.split('.'))
	}

	private def addTerminatingPlaces(pn: String, nbells: Int) =
	{
		var padded = pn;
		if (pn.length > 0)
		{
			val first = Rounds.indexOf(pn(0)) + 1
			if (first > 0 && first % 2 == 0)
				padded = "1" + padded
			val last = Rounds.indexOf(pn(pn.length - 1)) + 1
			if (last > 0 && last % 2 != nbells % 2)
				padded = padded + Rounds(nbells - 1)
		}
		padded
	}

	def output(xs: Seq[PN]): String =
	{
		xs.mkString(".").replace(".-", "-").replace("-.", "-")
	}

	def reverse(xs: Seq[PN], nbells: Int): Seq[PN] = xs.reverse.map(_.reverse(nbells))

	def generateLastRow(start: Row, pn: Seq[PN]): Row =
	{
		var r = start
		for (i <- 0 until pn.size)
			r = r.apply(pn(i))
		r
	}

	/**
	 * Fills the buffer with the rows generated by the pn list, starting with the start row, but excluding
	 * the finish row, which is instead returned.
	 * @param start
	 * @param pn
	 * @param builder
	 * @return
	 */
	def generateChanges(start: Row, pn: Seq[PN], builder: mutable.Buffer[Row]): Row =
	{
		var r = start
		for (i <- 0 until pn.size)
		{
			builder += r
			r = r.apply(pn(i))
		}
		r
	}

	/**
	 * Fills the buffer with the rows generated by the pn list, excluding the start row, but including
	 * the finish row, which is also returned.
	 * @param start
	 * @param pn
	 * @param builder
	 * @return
	 */
	def generateChangesExcludingStart(start: Row, pn: Seq[PN], builder: mutable.Buffer[Row]): Row =
	{
		var r = start
		for (i <- 0 until pn.size)
		{
			r = r.apply(pn(i))
			builder += r
		}
		r
	}

	/**
	 * Returns a list of rows generated from the given start row by the pn list; does not include the start row,
	 * but does include the finish row.
	 * TODO maybe return a Vector instead.
	 * @param start
	 * @param pn
	 * @return
	 */
	def generateRows(start: Row, pn: Seq[PN]): List[Row] =
	{
		val buf = ListBuffer[Row]()
		generateChangesExcludingStart(start, pn, buf)
		buf.toList
	}

	def generateAll(nbells: Int): Seq[PN] =
	{
		assert(nbells <= MaxStage)
		generateAll(0, nbells).tail.map(PN(_))
	}

	def generateAll(offset: Int, n: Int): List[String] =
	{
		def addToEachTail(head: Char, tails: List[String]) = tails.map(head + _)

		if (n <= 0)
			Nil
		else if (n == 1)
			List("" + Rounds(offset))
		else if (n == 2)
			List(Rounds.substring(offset, offset + 2), "")
		else
			addToEachTail(Rounds(offset), generateAll(offset + 1, n - 1)) ++ generateAll(offset + 2, n - 2)
	}

	/** Counts the number of negative rows generated by the given PN list (assuming we start from +ve) */
	def nNegativeRowsGenerated(pns: Seq[PN], nbells: Int) =
	{
		var nNegative = 0
		var sign = true
		for (pn <- pns)
		{
			if (pn.swapsSign(nbells))
				sign = !sign
			if (!sign)
				nNegative += 1
		}
		nNegative
	}

	/** Test */
	def main(args: Array[String])
	{
		val pnX = PN("x")
		val pn1256 = PN("1256")
		val pn14 = PN("14")
		val pn38 = PN("38")
		assert(!pn1256.allowsSwap(1))
		assert(!pn14.allowsSwap(1))
		assert(!pnX.allowsSwap(2))
		assert(pnX.allowsSwap(1))
		assert(pn38.allowsSwap(1))
		assert(pn1256.isPlace(1))
		assert(pn14.isPlace(1))
		assert(!pnX.isPlace(2))
		assert(!pnX.isPlace(1))
		assert(!pn38.isPlace(1))
		assert(pnX.oddPairsCrossing(8)==0)
		assert(pn1256.oddPairsCrossing(6)==0)
		assert(pn38.oddPairsCrossing(8)==4)
		assert(PN("1450").oddPairsCrossing(10)==6)
	}
}