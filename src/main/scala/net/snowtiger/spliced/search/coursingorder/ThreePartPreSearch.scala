package net.snowtiger.spliced.search.coursingorder

/**
 * @author mark
 */

object ThreePartPreSearch extends CoursingOrderSearch
{
	val GoodCO = Set("5xxx6", "xxx56", "56xxx", "6xxx5", "65xxx", "x65xx", "xx65x", "6xx5x", "5xx6x", "x5xx6", "x56xx", "xx56x").map{ CoursingOrder(_) }
	val OkCO = Set("5x6xx", "xx5x6", "6x5xx", "x6xx5").map{ CoursingOrder(_) } + ExtraCO
	val ExtraCO = CoursingOrder("xxx65")

	val AllCalls = List(Before(), Middle(), Wrong(), Home())

	val Start = CoursingOrder("5xxx6")

	def main(args: Array[String])
	{
		search(Start, Set(Start), Nil, 1)
	}

	def search(current: CoursingOrder, found: Set[CoursingOrder], revCalls: List[CallType], score: Int)
	{
		def output(calls: List[CallType]) { println(""+score+"/"+found.size+" "+calls.mkString(" ")) }

		for (call <- AllCalls)
		{
			val newCO = call.permute(current)
			if (newCO==Start)
			{
				if (score==GoodCO.size && found.contains(ExtraCO))
					output((call::revCalls).reverse)
			}
			else if (!found.contains(newCO))
			{
				if (GoodCO.contains(newCO))
					search(newCO, found+newCO, call::revCalls, score+1)
				else if (OkCO.contains(newCO))
					search(newCO, found+newCO, call::revCalls, score)
			}
		}
	}
}

