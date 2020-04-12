package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row

import scala.collection.BitSet

/**
 * @author mark
 */
case class Atw(methodMap: Map[ProtoMethod, SingleMethodAtw])
{
	def this() = this(Map())

	def get(method: ProtoMethod) = methodMap.getOrElse(method, new SingleMethodAtw(method))

	def add(method: ProtoMethod, lh: Row) =
		Atw( methodMap + (method -> (get(method)+lh)) )

	def add(methodAtw: SingleMethodAtw) =
		Atw( methodMap + (methodAtw.method -> (get(methodAtw.method)+methodAtw)) )

	def add(methodAtwList: List[SingleMethodAtw]): Atw =
		methodAtwList match
		{
			case Nil => this
			case matw::rest => add(matw).add(rest)
		}

	def add(other: Atw): Atw = add(other.methodMap.values.toList)

	def score = methodMap.values.map{_.score}.sum

}

case class SingleMethodAtw(method: ProtoMethod, flags: BitSet)
{
	def this(method: ProtoMethod) = this(method, BitSet())

	def +(lh: Row) =
	{
		val n = method.nbells-1
		var bits = BitSet()
		for (pos <- 2 to n+1)
			if (!lh.isMasked(pos))
				bits+= n*(lh.bellAt(pos)-2) + (pos-2)
		SingleMethodAtw(method, bits|flags)
	}

	def ++(lhs: Iterable[Row]) =
	{
		var result = this
		for (lh <- lhs)
			result+= lh
		result
	}

	def +(other: SingleMethodAtw) = SingleMethodAtw(method, flags|other.flags)

	def score = flags.size
}
