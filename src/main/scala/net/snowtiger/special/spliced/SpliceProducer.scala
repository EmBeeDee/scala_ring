package net.snowtiger.special.spliced

/**
 * @author mark
 */

object SpliceProducer
{
	val nBells = 8
	val mod = nBells-1
	val lhGroups = Map("a"->1, "b"->2, "c"->3, "d"-> -3, "e"-> -2, "f"-> -1)

	def main(args: Array[String]): Unit =
	{
		val all = solve(lhGroups.keySet.toList.sorted).map(normalise).toSet.toList.sortWith(sorter)
		val groups = all.groupBy(_.sorted)
		val solutions = groups.values.toList.sortBy((xs)=> xs.size*100+xs.head.size*10+xs.head.toSet.size)
		println("Size, # lhgroups, solutions(COM)")
		for (solution <- solutions)
			println(solution.head.size+"\t"+solution.head.toSet.size+"\t"+solution.map(_.mkString).map((s)=> s+"("+com(s)+")").mkString(" "))
	}

	def com(solution: String): Int =
		(solution+solution(0)).toList.sliding(2).map((xs)=> if (xs.head==xs.tail.head) 0 else 1).sum

	def sorter(xs: List[String], ys: List[String]): Boolean =
	{
		if (xs.size!=ys.size)
			return xs.size>=ys.size
		val distinctCompare = xs.toSet.size.compareTo(ys.toSet.size)
		if (distinctCompare!=0)
			return distinctCompare>0
		return xs.mkString.compareTo(ys.mkString)<=0;
	}

	def normalise(solution: List[String]): List[String] =
	{
		(0 until solution.length).map((i)=> solution.drop(i)++solution.take(i)).toList.sortBy(_.mkString).head
	}

	def solve(lhs: List[String]): List[List[String]] =
	{
		if (lhs.isEmpty)
			Nil
		else
		{
			val lh = lhs.head
			val next = (mod+lhGroups(lh))%mod
			solve(lhs.toSet, next, Set(next)).map(lh::_) ++ solve(lhs.tail)
		}
	}

	def solve(lhs: Set[String], current: Int, visited: Set[Int]): List[List[String]] =
	{
		var results:List[List[String]] = Nil
		for (lh <- lhs)
		{
			val next = (current+mod+lhGroups(lh))%mod
			if (next==0)
				results = List(lh)::results
			else if (!visited(next))
				results = solve(lhs, next, visited+next).map(lh::_) ++ results
		}
		results
	}

}