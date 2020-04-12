package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.atw.construct.TreblePath

import scala.collection.BitSet
import scala.collection.mutable.ListBuffer

/**
 * Only workable for 8 bells - a bitset recording one or more rows for a single treble position.
 * 5040 possible rows gives a maximum bitset size of 79 words. Seems pretty fast.
 *
 * @author mark
 */
case class RowBitSet(treblePos: Int, set: BitSet)
{
	lazy val size = set.size
	def trueWith(other: RowBitSet) = treblePos!=other.treblePos || (set&other.set).isEmpty
	def +(other: RowBitSet): RowBitSet = {assert(treblePos==other.treblePos); RowBitSet(treblePos, set|other.set)}
	def +(other: Option[RowBitSet]): RowBitSet = if (other.isDefined) this+other.get else this
	def -(other: RowBitSet): RowBitSet = {assert(treblePos==other.treblePos); RowBitSet(treblePos, set&~other.set)}
}

object RowBitSet
{
	val rowsByTreblePosition = Row.generateAll(8).groupBy{_.placeOf(1)}
	val numberedRows = rowsByTreblePosition.mapValues{_.zipWithIndex.toMap}.view.force
	val rowsByNumber = numberedRows.mapValues{_.map(_.swap)}.view.force
	def apply(treblePos: Int) = new RowBitSet(treblePos, BitSet())
	def apply(row: Row): RowBitSet =
	{
		val treblePos = row.placeOf(1)
		RowBitSet(treblePos, BitSet(numberedRows(treblePos)(row)))
	}
	def apply(rows: Iterable[Row]): RowBitSet = rows.map{RowBitSet(_)}.reduceLeft{(a,b)=> a+b}
}

/** Just like a RowBitSet, but holds row numbers in an ordinary set. May be more efficient for small row sets. */
case class RowSet(treblePos: Int, set: Set[Short])
{
	lazy val size = set.size
	def trueWith(other: RowSet) = treblePos!=other.treblePos || (set&other.set).isEmpty
	def +(other: RowSet): RowSet = {assert(treblePos==other.treblePos); RowSet(treblePos, set|other.set)}
	def +(other: Option[RowSet]): RowSet = if (other.isDefined) this+other.get else this
	def -(other: RowSet): RowSet = {assert(treblePos==other.treblePos); RowSet(treblePos, set&~other.set)}
}

object RowSet
{
	val shortSet: Set[Short] = Set()
	def apply(treblePos: Int) = new RowSet(treblePos, Set())
	def apply(row: Row): RowSet =
	{
		val treblePos = row.placeOf(1)
		// Use same row numbering as RowBitSet
		RowSet(treblePos, Set(RowBitSet.numberedRows(treblePos)(row).toShort))
	}
	def apply(rows: Iterable[Row]): RowSet = rows.map{RowSet(_)}.reduceLeft{(a,b)=> a+b}
	def apply(rbs: RowBitSet): RowSet = RowSet(rbs.treblePos, shortSet++rbs.set.map{_.toShort})
}

/**
 * A RowSet optimised for storage of small numbers of rows - uses a sorted Array of shorts, and does
 * not even reserve space for the treblePos (which must be held separately - the row numbers in the
 * internal array are only unique for a given treble position).
 */
class BasicRowArray protected (protected val arr: Array[Short])
{
	def size = arr.size

	def trueWith(other: BasicRowArray) = !intersects(other.arr)

	def containsAll(other: BasicRowArray) = combine(other.arr).sameElements(arr)

	private def intersects(other: Array[Short]): Boolean =
	{
		var i = 0
		var j = 0
		while (i<arr.size && j<other.size && arr(i)!=other(j))
		{
			// Move our pointer on until our head is greater or equal to the other's head, or has reached array end
			while (i<arr.size && arr(i)<other(j))
				i+= 1
			// If our array has elements left, and heads not equal, move the other pointer on until it is greater or equal
			if (i<arr.size)
				while (j<other.size && other(j)<arr(i))
					j+= 1
		}
		// We've found a collision if neither pointer is at the array end
		val collision = i<arr.size && j<other.size
		collision
	}

	private def combine(other: Array[Short]): Array[Short] =
	{
		var result = List[Short]()
		var i = 0
		var j = 0
		while (i<arr.size && j<other.size)
		{
			// Pick the smallest head and at it to the new list
			if (arr(i) <= other(j))
			{
				result = arr(i) :: result
				// If two heads equal, copy only one, bump both array pointers
				if (arr(i) == other(j))
					j+= 1
				i += 1
			}
			else
			{
				result = other(j) :: result
				j += 1
			}
		}
		// Only one array can have elements left, exhaust whichever one it is
		while (i<arr.size)
		{
			result = arr(i) :: result
			i += 1
		}
		while (j<other.size)
		{
			result = other(j) :: result
			j += 1
		}
		result.reverse.toArray
	}

	override def hashCode() =
	{
		var hash = 0
		var i = 0
		while (i<arr.size && i<5)
		{
			hash = 31 * hash + arr(i)
			i+= 1
		}
		hash
	}

	override def equals(obj: scala.Any) = obj match
	{
		case other: BasicRowArray => arr.sameElements(other.arr)
		case _ => false
	}

	override def toString = "BasicRowArray("+arr.size+")"
}

object BasicRowArray
{
	def apply(rbs: RowBitSet): BasicRowArray =
	{
		val arr = rbs.set.toArray.map{_.toShort}.sorted
		new BasicRowArray(arr)
	}
}

/** A RowSet optimised for storage of small numbers of rows - uses a sorted Array of shorts */
class RowArray private (val treblePos: Int, arr: Array[Short]) extends BasicRowArray(arr)
{
	def trueWith(other: RowArray) = treblePos==other.treblePos && super.trueWith(other)

	def containsAll(other: RowArray) = treblePos==other.treblePos && super.containsAll(other)

	override def hashCode() = super.hashCode() + 31 * treblePos

	override def equals(obj: scala.Any) = obj match
	{
		case other: RowArray => treblePos == other.treblePos && arr.sameElements(other.arr)
		case _ => false
	}

	def toRows: Array[Row] = arr.map{RowBitSet.rowsByNumber(treblePos)(_)}

	override def toString = "RowArray("+treblePos+","+arr.size+")"
}

object RowArray
{
	def apply(rows: Iterable[Row]): RowArray =
	{
		val treblePos = rows.head.placeOf(1)
		// Use same row numbering as RowBitSet
		val arr = rows.toArray.map{(r)=> RowBitSet.numberedRows(treblePos)(r).toShort}.sorted
		new RowArray(treblePos, arr)
	}

	def apply(rbs: RowBitSet): RowArray =
	{
		val treblePos = rbs.treblePos
		val arr = rbs.set.toArray.map{_.toShort}.sorted
		new RowArray(treblePos, arr)
	}
}

/** A map of RowBitSets, covering one or more treble positions */
case class MultiBitSet(treblePosSets: Map[Int, RowBitSet])
{
	lazy val size = treblePosSets.values.map{_.size}.sum
	def trueWith(other: RowBitSet): Boolean = treblePosSets.get(other.treblePos) match
	{
		case Some(rbs) => (rbs.set&other.set).isEmpty
		case None => true
	}
	def trueWith(other: MultiBitSet): Boolean = treblePosSets.keys.forall((tp)=> other.trueWith(treblePosSets(tp)))
	def +(other: RowBitSet): MultiBitSet =
		MultiBitSet(treblePosSets.updated(other.treblePos, other+treblePosSets.get(other.treblePos)))
	def +(other: MultiBitSet): MultiBitSet =
		MultiBitSet(treblePosSets++(other.treblePosSets.mapValues{(rbs)=> rbs+treblePosSets.get(rbs.treblePos)}.view.force))
	def -(other: RowBitSet): MultiBitSet = treblePosSets.get(other.treblePos) match
	{
		case Some(us) => MultiBitSet(treblePosSets.updated(other.treblePos, us-other))
		case None => this
	}
	def -(other: MultiBitSet): MultiBitSet = MultiBitSet(treblePosSets++
			(other.treblePosSets.filterKeys{treblePosSets.contains(_)}.mapValues{(rbs)=> treblePosSets(rbs.treblePos)-rbs}.view.force))
	def partial(maxTreblePos: Int): MultiBitSet =	MultiBitSet(treblePosSets.filterKeys(_<=maxTreblePos))	// TODO force this view or not?
}

object MultiBitSet
{
	def apply(): MultiBitSet = MultiBitSet(Map[Int,RowBitSet]())
	def apply(rbs: RowBitSet): MultiBitSet = MultiBitSet(Map(rbs.treblePos->rbs))
	def apply(rows: Iterable[Row]): MultiBitSet = MultiBitSet(rows.groupBy(_.placeOf(1)).mapValues{RowBitSet(_)}.view.force)
	def addAll(mbsList: Iterable[MultiBitSet]): MultiBitSet = mbsList.reduceLeft{(a,b)=> a+b}
}

/** Uses an array instead of a map - better performance? */
case class MultiBitSet2(sets: Array[BitSet])
{
	lazy val size = sets.map{_.size}.sum
	def trueWith(other: RowBitSet): Boolean = other.treblePos>sets.length || (other.set&sets(other.treblePos-1)).isEmpty
	def trueWith(other: MultiBitSet2): Boolean = sets.zip(other.sets).forall{(p)=> (p._1&p._2).isEmpty}
	def +(other: RowBitSet): MultiBitSet2 =
	{
		val updatePos = other.treblePos-1
		if (other.treblePos>sets.length)
		{
			val newSets = new Array[BitSet](other.treblePos)
			Array.copy(sets, 0, newSets, 0, sets.length)
			newSets(updatePos) = other.set
			MultiBitSet2(newSets)
		}
		else
		{
			val newSets = sets.clone()
			newSets(updatePos) = newSets(updatePos)|other.set
			MultiBitSet2(newSets)
		}
	}
	def +(other: MultiBitSet2): MultiBitSet2 =
	{
		if (other.sets.length>sets.length)
			other+this
		else
		{
			val newSets = sets.clone()
			for (i <- 0 until other.sets.length)
				newSets(i) = newSets(i)|other.sets(i)
			MultiBitSet2(newSets)
		}
	}
	def -(other: RowBitSet): MultiBitSet2 =
	{
		if (other.treblePos>sets.length)
			this
		else
		{
			val updatePos = other.treblePos-1
			val newSets = sets.clone()
			newSets(updatePos) = newSets(updatePos)&~other.set
			MultiBitSet2(newSets)
		}
	}
	def -(other: MultiBitSet2): MultiBitSet2 =
	{
		val newSets = sets.clone()
		for (i<- 0 until sets.length.min(other.sets.length))
			newSets(i) = newSets(i)&~other.sets(i)
		MultiBitSet2(newSets)
	}
	def partial(nSections: Int): MultiBitSet2 =
	{
		val maxTreblePos = nSections*2
		if (sets.length<=maxTreblePos)
			this
		else
		{
			val newSets = new Array[BitSet](maxTreblePos)
			Array.copy(sets, 0, newSets, 0, maxTreblePos)
			MultiBitSet2(newSets)
		}
	}
}

case class MethodNodeBitSet(course: MethodCourse, method: SearchMethod) extends TreblePath
{
	/** Reduce this to ignore falseness beyond a given treble position */
	val maxUpTo = 8
	/** Note that section end 0 is actually the 1st section head, i.e. the leadhead */
	val (bitset,isTrue,sectionEnds) = buildBitSet
	def trueUpTo(other: MultiBitSet): Int = trueUpTo(other, 0)
	def trueUpTo(other: MultiBitSet, from: Int): Int =
	{
		var isTrue = true
		var tp = from
		while (tp<maxUpTo && isTrue)
		{
			tp+= 1
			val rbs = bitset.treblePosSets.get(tp)
			if (rbs.isDefined)
				isTrue = other.trueWith(rbs.get)
			else
				tp = maxUpTo
		}
		if (isTrue) maxUpTo/2 else (tp-1)/2
	}
	private def buildBitSet =
	{
		val buf = ListBuffer[Row]()
		for (lh <- course.leadheads)
			method.generateLead(lh, buf)
		val finalTreblePos = buf(method.leadLength/2-1).placeOf(1)
		val sectionEnds = SectionEndOffsets.slice(0,finalTreblePos/2+1).map{extractSectionEnds(_,buf)}
		(MultiBitSet(buf), buf.toSet.size==buf.size, sectionEnds)
	}
	private def extractSectionEnds(from: Int, buf: ListBuffer[Row]) =
	{
		val offset1 = from
		val offset2 = method.offsetTo2ndHalflead(offset1)
		var i = 0
		val sectionEnds = new ListBuffer[Row]()
		for (lh <- course.leadheads)
		{
			sectionEnds += buf(i+offset1)
			sectionEnds += buf(i+offset2)
			i+= method.leadLength
		}
		sectionEnds.toList
	}

	override def toString = method.name+" ("+method.lhGroup+")"
}

case class PartialMethodNodeBitSet(node: MethodNodeBitSet, nSections: Int) extends TreblePath
{
	val effectiveSections = if (node.method.leadLength==2) 0 else nSections
	val rowReached = SectionLengths(effectiveSections)
	val bitset = node.bitset.partial(rowReached)
	def totalRows = 7*2*rowReached
	def lastPN = if (effectiveSections==0) List(PN("12")) else node.method.pn.slice(0,rowReached-1).reverse
	def nameIfComplete = if (effectiveSections==4) Some(node.toString) else None
	def sectionEnds = node.sectionEnds(effectiveSections)
	def maximize(other: MultiBitSet): (PartialMethodNodeBitSet,MultiBitSet) =
	{
		val maximizedSections = node.trueUpTo(other, nSections*2)
		if (maximizedSections==nSections)
			(this, other)
		else
		{
			var newOther = other
			for (i <- nSections*2 until maximizedSections*2)
				newOther = newOther+node.bitset.treblePosSets(i+1)
			(PartialMethodNodeBitSet(node, maximizedSections), newOther)
		}
	}
	override def toString = node.toString+(if (effectiveSections<4) "/"+effectiveSections else "")
}

case class SectionBitSet(lo: RowBitSet, hi: RowBitSet)
{
	assert(lo.treblePos+1==hi.treblePos)
	def trueWith(other: SectionBitSet) = lo.treblePos!=other.lo.treblePos || 
			(lo.trueWith(other.lo) && hi.trueWith(other.hi))
	def +(other: SectionBitSet) = SectionBitSet(lo+other.lo, hi+other.hi)
}