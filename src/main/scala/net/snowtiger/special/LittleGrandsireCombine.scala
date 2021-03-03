package net.snowtiger.special

import scala.io.Source

/**
 * @author mark
 */

object LittleGrandsireCombine
{
	val tittumsCallings = Source.fromFile("tittums.txt").getLines().toList.map(_.tail)
	val handstrokeCallings = Source.fromFile("handstroke.txt").getLines().toList

	def callingLength(calling: String): Int = calling.replaceAll("\\s", "").length

	val tittumsByLength = tittumsCallings.groupBy(callingLength)

	def main(args: Array[String]): Unit =
	{
		def flat(callings: Option[List[String]]): List[String] = if (callings.isDefined) callings.get else Nil

		for (handstrokeCalling <- handstrokeCallings)
		{
			val length = callingLength(handstrokeCalling)
			val minRequiredTittumsLenth = 102-length
			if (minRequiredTittumsLenth>length)
			{
				findPrefixes(handstrokeCalling, flat(tittumsByLength.get(minRequiredTittumsLenth)))
				findPrefixes(handstrokeCalling, flat(tittumsByLength.get(minRequiredTittumsLenth+1)))
				findPrefixes(handstrokeCalling, flat(tittumsByLength.get(minRequiredTittumsLenth+2)))
				findPrefixes(handstrokeCalling, flat(tittumsByLength.get(minRequiredTittumsLenth+3)))
				findPrefixes(handstrokeCalling, flat(tittumsByLength.get(minRequiredTittumsLenth+4)))
			}
		}
	}

	def findPrefixes(handstrokeCalling: String, backstrokeCallings: List[String]): Unit =
	{
		for (backstrokeCalling <- backstrokeCallings)
		{
			//val n = commonPrefixLength(handstrokeCalling, backstrokeCalling)
			val commonSubstrings = longestCommonSubstringsFast(handstrokeCalling, backstrokeCalling)
			val n = if (commonSubstrings.isEmpty) 0 else if (commonSubstrings.get.isEmpty) 0 else commonSubstrings.get.head.length
			if (n>40)
			{
				println(n+" "+commonSubstrings.get.mkString(" / "))
				println(handstrokeCalling)
				println(backstrokeCalling)
				println
			}
		}
	}

	def commonPrefixLength(str1: String, str2: String): Int =
	{
		if (str1.isEmpty || str2.isEmpty)
			0
		else if (str1.head==str2.head)
			1 + commonPrefixLength(str1.tail, str2.tail)
		else
			0
	}

	def longestCommonSubstringsFast(left: String, right: String): Option[Set[String]] =
		if (left.nonEmpty && right.nonEmpty) {
			val (shorter, longer) =
				if (left.length < right.length) (left, right)
				else (right, left)

			@scala.annotation.tailrec
			def recursive(
											 indexLonger: Int = 0,
											 indexShorter: Int = 0,
											 currentLongestLength: Int = 0,
											 lengthsPrior: List[Int] = List.fill(shorter.length)(0),
											 lengths: List[Int] = Nil,
											 accumulator: List[Int] = Nil
											 ): (Int, List[Int]) =
				if (indexLonger < longer.length) {
					val length =
						if (longer(indexLonger) != shorter(indexShorter)) 0
						else lengthsPrior.head + 1
					val newCurrentLongestLength =
						if (length > currentLongestLength) length
						else currentLongestLength
					val newAccumulator =
						if ((length < currentLongestLength) || (length == 0)) accumulator
						else {
							val entry = indexShorter - length + 1
							if (length > currentLongestLength) List(entry)
							else entry :: accumulator
						}
					if (indexShorter < shorter.length - 1)
						recursive(
							indexLonger,
							indexShorter + 1,
							newCurrentLongestLength,
							lengthsPrior.tail,
							length :: lengths,
							newAccumulator
						)
					else
						recursive(
							indexLonger + 1,
							0,
							newCurrentLongestLength,
							0 :: lengths.reverse,
							Nil,
							newAccumulator
						)
				}
				else (currentLongestLength, accumulator)

			val (length, indexShorters) = recursive()
			if (indexShorters.nonEmpty)
				Some(
					indexShorters
							.map {
						indexShorter =>
							shorter.substring(indexShorter, indexShorter + length)
					}
							.toSet
				)
			else None
		}
		else None
}