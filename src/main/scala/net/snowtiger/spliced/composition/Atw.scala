package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{NamedMethod, Row}

import scala.collection.{BitSet, mutable}

/**
 * Single-method ATW tracker; treble-dominated methods only.
 * @author mark
 */
trait Atw[T <: Atw[T]] extends Addable[T]
{ this: T =>

	val method: NamedMethod
	val nCombinations = (method.nbells-1)*(method.nbells-1)

	def +(lh: Row): T
	def +(other: T): T

	def atwScore: Int
	def isAtw = atwScore==nCombinations
}

/**
 * For treble-dominated methods only. Minimises storage space by using a bit array to track
 * whether each bell has rung a place bell. Immutable: + method returns a new instance.
 *
 * @author mark
 */
case class BitAtw(val method: NamedMethod, val flags: BitSet) extends Atw[BitAtw]
{
	def this(method: NamedMethod) = this(method, BitSet())

	def +(lh: Row) =
	{
		val n = method.nbells-1
		var bits = BitSet()
		for (pos <- 2 to n+1)
			bits+= n*(lh.bellAt(pos)-2) + (pos-2)
		BitAtw(method, bits|flags)
	}

	def +(other: BitAtw) = BitAtw(method, flags|other.flags)

	def atwScore = flags.size
}

object BitAtw
{
	def apply(method: NamedMethod, lh: Row): BitAtw = new BitAtw(method) + lh
}

trait FullyCountedAtw[T <: FullyCountedAtw[T]] extends Atw[T]
{ this: T =>

	def getCount(key: (Char,Int)):Int

	def -(other: T): T

	def doubleAtwScore: Int
	def isDoubleAtw = doubleAtwScore==2*nCombinations
}

/**
 * For treble-dominated methods only. Keeps full counts of how many place bells each bell rings.
 * Mutable, + method returns the same instance.
 *
 * @author mark
 */
class MutableFullyCountedAtw(val method: NamedMethod) extends FullyCountedAtw[MutableFullyCountedAtw]
{
	val counts = mutable.Map[(Char, Int), Int]()

	override def getCount(key: (Char,Int)):Int = counts.getOrElse(key, 0)

	def +(lh: Row) =
	{
		var pos = 1
		for (bell <- lh.toString.tail)
		{
			pos+= 1
			val key = (bell,pos)
			counts+= key -> (getCount(key)+1)
		}
		this
	}

	def +(other: MutableFullyCountedAtw) =
	{
		for ((key, count) <- other.counts)
			counts+= key -> (getCount(key) + count)
		this
	}

	def -(other: MutableFullyCountedAtw) =
	{
		for ((key, count) <- other.counts)
			counts+= key -> (getCount(key) - count)
		this
	}

	def atwScore = counts.size

	def doubleAtwScore = counts.size + counts.values.filter(_>1).size
}

/**
 * For treble-dominated methods only. Keeps full counts of how many place bells each bell rings.
 * Immutable version.
 *
 * @author mark
 */
case class ImmutableFullyCountedAtw(val method: NamedMethod, val counts: Map[(Char, Int), Int]) extends FullyCountedAtw[ImmutableFullyCountedAtw]
{
	def this(method: NamedMethod) = this(method, Map())

	override def getCount(key: (Char,Int)):Int = counts.getOrElse(key, 0)

	def +(lh: Row): ImmutableFullyCountedAtw =
	{
		var newCounts = counts
		for (key <- lh.toString.zipWithIndex.tail)
			newCounts+= key -> (getCount(key) + 1)
		ImmutableFullyCountedAtw(method, newCounts)
	}

	def +(other: ImmutableFullyCountedAtw): ImmutableFullyCountedAtw =
	{
		var newCounts = counts
		for ((key, count) <- other.counts)
			if (count>0)
				newCounts+= key -> (getCount(key) + count)
		ImmutableFullyCountedAtw(method, newCounts)
	}

	def -(other: ImmutableFullyCountedAtw): ImmutableFullyCountedAtw =
	{
		var newCounts = counts
		for ((key, count) <- other.counts)
			if (count>0)
			{
				// Must remove (bell,place) key from map if count goes to zero
				val newCount = getCount(key) - count
				if (newCount<=0)
					newCounts-= key
				else
					newCounts+= key -> newCount
			}
		ImmutableFullyCountedAtw(method, newCounts)
	}

	def atwScore = counts.size

	def doubleAtwScore = counts.size + counts.values.filter(_>1).size
}

object ImmutableFullyCountedAtw
{
	def apply(method: NamedMethod, lh: Row) = new ImmutableFullyCountedAtw(method) + lh
}