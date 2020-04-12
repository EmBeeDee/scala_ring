package net.snowtiger.spliced.composition

import scala.reflect.ClassTag

/**
 * @author mark
 */
trait ReplaceableArray[T]
{
	def getTotal: T

	def replace(i: Int, el: T)

}

trait Addable[T <: Addable[T]]
{
	def +(other: T): T
}

trait Subtractable[T <: Subtractable[T]] extends Addable[T]
{
	def -(other: T): T
}

/**
 * Uses the "subtraction" optimisation to keep track of the sum of all elements in an array, with
 * the sum recalculated in constant time if a single element is updated, by subtracting the value
 * of the previous element, and adding the value of the new element. This is the fastest way to
 * track the total value, but depends on the element supporting subtraction as well as addition.
 *
 * @author mark
 */
class FastReplacingArray[T <: Subtractable[T] : ClassTag](base: Array[T]) extends ReplaceableArray[T]
{
	var total:T = base.reduceLeft{ (a,b)=> a+b }

	def getTotal = total

	def replace(i: Int, el: T)
	{
		total = total-base(i)
		base(i) = el
		total = total+base(i)
	}

}

/**
 * Uses the "pyramid" optimisation to keep track of the sum of all elements in an array, with
 * the sum recalculated in the minimum number of steps O(log N) if a single element is updated.
 * This is achieved by building a pyramid of worker arrays, each half the size of the one below
 * it, and where element i in a worker array corresponds to the sum of elements i*2 and i*2+1 in
 * the array below it.
 *
 * @author mark
 */
class BinarySummedArray[T <: Addable[T] : ClassTag](base: Array[T]) extends ReplaceableArray[T]
{
	private val pyramidSize:Int =
	{
		var size = 1
		var len = base.size
		while (len>1)
		{
			len = (len+1)/2
			size+= 1
		}
		size
	}

	val pyramid = new Array[Array[T]](pyramidSize)
	buildPyramid()

	private def buildPyramid()
	{
		pyramid(0) = base.toArray

		def build(level: Int)
		{
			val levelSize = pyramid(level).size
			if (levelSize>1)
			{
				val nextLevel = level+1
				val nextLevelSize = (levelSize+1)/2
				pyramid(nextLevel) = new Array[T](nextLevelSize)
				for (i <- 0 until levelSize-1 by 2)
					pyramid(nextLevel)(i/2) = pyramid(level)(i) + pyramid(level)(i+1)
				if (levelSize%2==1)
					pyramid(nextLevel)(nextLevelSize-1) = pyramid(level)(levelSize-1)
				build(nextLevel)
			}
		}

		build(0)
	}

	def getTotal = pyramid(pyramidSize-1)(0)

	def replace(i: Int, el: T)
	{
		def replace(level: Int, i: Int, el: T)
		{
			pyramid(level)(i) = el
			val levelSize = pyramid(level).size
			val j = i/2
			if (levelSize>1)
			{
				if (i==levelSize-1)
					replace(level+1, j, el)
				else
					replace(level+1, j, pyramid(level)(j*2)+pyramid(level)(j*2+1))
			}
		}

		replace(0, i, el)
	}
}
