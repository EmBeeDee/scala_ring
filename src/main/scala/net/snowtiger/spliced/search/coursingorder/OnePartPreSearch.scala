package net.snowtiger.spliced.search.coursingorder

/**
 * @author mark
 */

object OnePartPreSearch extends CoursingOrderSearch
{
	//val ToRemove = Set("13265478", "12465378", "16354278")	// Gives 5 comps @ 41 - "callings41a"
	//val ToRemove = Set("13265478", "12643578", "16354278")	// Gives
	//val ToRemove = Set("13624578", "15642378", "15463278", "16354278")	// 10 comps @40 - "callings40a"
	/*
	val ToRemove = Set()
	val GoodCO = (Set("12345678", "12436578", "13254678", "13526478", "13425678", "14536278", "14235678", "13246578",
		"14523678", "14352678", "14632578", "14365278", "14256378", "13542678", "13456278", "14326578",	"16452378",
		"15346278", "15362478", "15432678", "16543278", "16435278", "15634278") ++
			Set("14625378", "13645278", "13462578", "12546378", "12534678", "12465378", "14263578", "12635478", "15426378",
		"12364578", "12453678", "15463278", "12356478", "15642378", "16354278", "16532478", "15243678", "13624578",
		"14562378",	"13265478", "12643578") ++
			Set("13245678", "12435678", "14325678", "12346578") --
			ToRemove).map{ coFromCH(_) }
	val OkCO = Set[String]().map{ CoursingOrder(_) }
	*/
	///*
	val GoodCO = Set("23456", "23564", "24536", "24653", "25346", "26435", "34256", "35426", "35426", "42356", "46532",
		"52436", "53246", "53462", "54326", "54632", "56234", "56342", "56423", "62345", "62453", "63425", "63542", "64235", "64352", "65324", "65432").map(CoursingOrder(_))
	val OkCO = CoursingOrder.positive
	//*/

	//val AllCalls = List(Before(), Middle(), Wrong(), Home())
	val AllCalls = List(Middle(), Wrong(), Home())

	val Start = CoursingOrder.Start
	//val Start = CO("ooe4e")

	def main(args: Array[String])
	{
		println("Good CO size = "+GoodCO.size)
		println("OK CO size = "+OkCO.size)
		searchOneVisit(Start, Set(Start), Nil, 1)
		//searchMultiVisit(Start, Map(), Map(), Set(Start), Home(), Nil)
	}

	val maxLen = 18
	val minGood = 15
	val maxOK = 1

	def printComp(calls: List[CallType]): String  =
	{
		var co = Start
		calls.map((x)=> {co = x.permute(co); co}).mkString(" ")
	}

	def searchOneVisit(current: CoursingOrder, found: Set[CoursingOrder], revCalls: List[CallType], score: Int)
	{
		def output(calls: List[CallType]) { println(""+score+"/"+found.size+" "+calls.mkString(" ")+" "+printComp(calls)) }

		if (found.size<maxLen)
			for (call <- AllCalls)
			{
				val newCO = call.permute(current)
				if (newCO==Start)
				{
					if (score>=minGood)
						output((call::revCalls).reverse)
				}
				else if (!found.contains(newCO) && score>=found.size-maxOK)
				{
					if (GoodCO.contains(newCO))
						searchOneVisit(newCO, found+newCO, call::revCalls, score+1)
					else if (OkCO.contains(newCO))
						searchOneVisit(newCO, found+newCO, call::revCalls, score)
				}
			}
	}

	/** This one allows coursing orders to be revisited */
	def searchMultiVisit(current: CoursingOrder, foundStart: Map[CallType,Set[CoursingOrder]], foundEnd: Map[CallType,Set[CoursingOrder]],
											 goodVisited: Set[CoursingOrder], lastCall: CallType, revComp: List[String])
	{
		def output(calls: List[String])
		{
			println(""+goodVisited.size+"/"+calls.size+" "+calls.mkString(" ")+" "+goodVisited.toList.map(_.raw).sorted.mkString(" "))
		}

		def search(callPos: OnePartPreSearch.CallType with Product with Serializable, in: Set[CoursingOrder], out: Set[CoursingOrder], callStr: String, newCO: CoursingOrder): Unit =
		{
			if (newCO == Start)
			{
				if (goodVisited.size>12 && (revComp.size+1).toDouble/goodVisited.size<1.3)
				//if (goodVisited.size>=18 && revComp.size+1<27)
					output((callStr :: revComp).reverse)
			}
			val newFoundStart = foundStart + (lastCall -> (in + current))
			val newFoundEnd = foundEnd + (callPos -> (out + current))
			if (GoodCO.contains(newCO))
				searchMultiVisit(newCO, newFoundStart, newFoundEnd, goodVisited + newCO, callPos, callStr :: revComp)
			else if (OkCO.contains(newCO))
				searchMultiVisit(newCO, newFoundStart, newFoundEnd, goodVisited, callPos, callStr :: revComp)
		}

		for (callPos <- AllCalls)
		{
			val in = foundStart.getOrElse(lastCall, Set[CoursingOrder]())
			val out = foundEnd.getOrElse(callPos, Set[CoursingOrder]())
			if (!in.contains(current) && !out.contains(current))
			{
				search(callPos, in, out, callPos.toString, callPos.permute(current))
				if (callPos.name!="B")
					search(callPos, in, out, callPos.toString+"'", callPos.permuteSingle(current))
			}
		}
	}
}