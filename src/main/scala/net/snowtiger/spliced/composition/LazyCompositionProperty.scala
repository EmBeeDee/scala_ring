package net.snowtiger.spliced.composition

import scala.reflect.ClassTag


/**
 * @author mark
 */

abstract class LazyCompositionProperty[T](fastComp: FastComp) extends ReplacementListener
{
	// Add ourself to the parent FastComp's listener list, so we get notified on node replacement
	fastComp.addReplacementListener(this)

	val nodes = fastComp.nodeArray
	var initialised = false
	var total:T = _

	def getTotal =
	{
		if (!initialised)
		{
			total = recalc()
			initialised = true
		}
		total
	}

	def replace(i: Int)
	{
		total = recalc()
	}

	protected def recalc():T
}

abstract class LazyReplaceableArray[T: ClassTag](fastComp: FastComp, deriver: (Int)=>T) extends LazyCompositionProperty[T](fastComp) with ReplaceableArray[T]
{
	var delegate: ReplaceableArray[T] = _

	override def replace(i: Int)
	{
		if (initialised)
			replace(i, deriver(i))
	}

	def replace(i: Int, el: T)
	{
		delegate.replace(i, el)
		total = delegate.getTotal
	}

	protected def recalc() =
	{
		val derivedArray = new Array[T](nodes.size)
		for (i <- 0 until nodes.size)
			derivedArray(i) = deriver(i)
		delegate = makeDelegate(derivedArray)
		total = delegate.getTotal
		total
	}

	protected def makeDelegate(base: Array[T]): ReplaceableArray[T]
}

class LazyFastArray[T <: Subtractable[T] : ClassTag](fastComp: FastComp, deriver: (Int)=>T) extends LazyReplaceableArray[T](fastComp, deriver)
{
	protected def makeDelegate(base: Array[T]) = new FastReplacingArray[T](base)
}

class LazyPyramidArray[T <: Addable[T] : ClassTag](fastComp: FastComp, deriver: (Int)=>T) extends LazyReplaceableArray[T](fastComp, deriver)
{
	protected def makeDelegate(base: Array[T]) = new BinarySummedArray[T](base)
}

trait ReplacementListener
{
	def replace(i: Int)
}

