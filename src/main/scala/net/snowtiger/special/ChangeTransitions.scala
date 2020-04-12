package net.snowtiger.special

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing._

/**
 * @author mark
 */

object ChangeTransitions
{
	def main(args: Array[String]): Unit =
	{
		val nbells = 6
		//val ct1 = new ChangeTransitions(12, true)
		//ct1.find(Row(nbells), GoodPn(nbells).noConsec)

		val ct2 = new ChangeTransitions(8, false)
		ct2.find(Row(nbells), GoodPn(nbells).callChangePNs)
	}
}

class ChangeTransitions(val maxSteps: Int, avoidConsec: Boolean)
{
	def find(from: Row, pns: List[PN]): Unit =
	{
		val allRows = Row.generateAll(from.nbells)
		val (solutions,failed) = allRows.map(find(from, _, pns, Nil, Set(from))).zip(allRows).partition(_._1.isDefined)
		val sortedSolutions = solutions.sortBy(_._1.get.size)
		for (solution <- sortedSolutions)
			println(from+"->"+solution._2+" "+solution._1.get.size+" "+PN.output(solution._1.get.reverse))
		for (solution <- failed)
			println(from+"->"+solution._2+" no paths")
	}

	def find(from: Row, to: Row, pns: List[PN], revBest: List[PN], found: Set[Row]): Option[List[PN]] =
	{
		if (from==to)
			Some(revBest)
		else if (revBest.size==maxSteps)
			None
		else
		{
			var best: Option[List[PN]] = None
			val it = pns.iterator
			val distLeft = from.distance(to)
			while ((best.isEmpty || best.get.size>1) && it.hasNext)
			{
				val pn = it.next()
				if (!avoidConsec || revBest.isEmpty || pn.acceptableConsecutive(revBest.head))
				{
					val next = from.apply(pn)
					if (!found(next))
						find(from.apply(pn), to, pns, pn::revBest, found+next) match
						{
							case Some(solution) => if (best.isEmpty || best.get.size>solution.size) best = Some(solution)
							case None =>
						}
				}
			}
			best
		}
	}
}