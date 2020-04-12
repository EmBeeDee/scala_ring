package net.snowtiger.spliced.search.coursingorder


/**
 * @author mark
 */

object TwoPartPreSearch extends CoursingOrderSearch
{
	val GoodCO = Set("4eooe", "e4eoo", "e4ooe", "o4eoe", "eo4oe", "ee4oo", "oo4ee", "ooe4e", "eoo4e", "eooe4", "eooe4").map{ CoursingOrder(_) }
	val OkCO = Set("e4oeo", "o4oee", "oe4eo", "oe4oe", "eo4eo", "oe4eo", "eoe4o", "oeo4e", "eeo4o").map{ CoursingOrder(_) }

	val AllCalls = List(Middle(), Wrong(), Home())

	val Start = CoursingOrder("ooe4e")
	//val Start = CO("ooe4e")

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
				if (score>=GoodCO.size-0)
					output((call::revCalls).reverse)
			}
			else if (!found.contains(newCO) && found.size<GoodCO.size+5)
			{
				if (GoodCO.contains(newCO))
					search(newCO, found+newCO, call::revCalls, score+1)
				else if (true)//OkCO.contains(newCO))
					search(newCO, found+newCO, call::revCalls, score)
			}
		}
	}

}