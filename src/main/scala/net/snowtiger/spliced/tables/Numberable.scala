package net.snowtiger.spliced.tables;

import scala.collection.mutable

/**
 * @author mark
 */
trait Numberable
{
	var n = -1
}

class Numberer[T <: Numberable]
{
	var n = 0
	val allocated = mutable.Map[T, T]()

	def getInterned(item: T) =
	{
		if (allocated.contains(item))
			allocated(item)
		else
		{
			println("BAD!!!")
			allocated(item)
		}
	}

	def intern(item: T) =
	{
		allocated.get(item) match
		{
			case None => {n+= 1; item.n = n; allocated+= item->item; item}
			case Some(existing) => existing
		}
	}

	def getAll = allocated.keySet

	def clear()
	{
		allocated.clear()
		n = 0
	}

	def size = n
}