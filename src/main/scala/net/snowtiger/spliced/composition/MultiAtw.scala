package net.snowtiger.spliced.composition

import net.snowtiger.ringing.NamedMethod

/**
 * Keeps track of ATW for multiple methods, using a Map of NamedMethod->BitAtw
 * @author mark
 */
case class MultiAtw(val methodAtw: Map[NamedMethod, BitAtw]) extends Addable[MultiAtw]
{
	def atwScore = methodAtw.values.map{_.atwScore}.sum
	def isAtw = methodAtw.values.forall(_.isAtw)

	def +(other: MultiAtw) =
	{
		var result = this
		for (atw <- other.methodAtw.values)
			result = result+atw
		result
	}

	def +(single: BitAtw) =
	{
		val m = single.method
		if (methodAtw.contains(m))
			MultiAtw(methodAtw + (m -> (single+methodAtw(m))))
		else
			MultiAtw(methodAtw + (m -> single))
	}
}

object MultiAtw
{
	def apply(): MultiAtw = MultiAtw(Map())
}