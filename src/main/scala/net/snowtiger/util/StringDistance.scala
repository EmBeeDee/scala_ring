package net.snowtiger.util

/**
 * @author mark
 */

object StringDistance
{
	def levenshtein(s1: String, s2: String): Int =
	{
		def sd(s1: List[Char], s2: List[Char], costs: List[Int]): Int = s2 match
		{
			case Nil => costs.last
			case c2 :: tail => sd(s1, tail,
				(List(costs.head + 1) /: costs.zip(costs.tail).zip(s1))((a, b) => b match
				{
					case ((rep, ins), chr) => Math.min(Math.min(ins + 1, a.head + 1), rep + (if (chr == c2) 0 else 1)) :: a
				}).reverse
			)
		}
		sd(s1.toList, s2.toList, (0 to s1.length).toList)
	}
}