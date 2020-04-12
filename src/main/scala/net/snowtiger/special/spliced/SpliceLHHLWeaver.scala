package net.snowtiger.special.spliced

import scala.collection.mutable.ListBuffer

/**
 * Finds suitable splices for a cyclic course rung in separate chunks, with one bit in reverse, as required by
 * Chris Poole's 2018 dinner-day composition.
 *
 * Note: C = c (short course), c = c1, D = d, d=d1, and the same for (J,j) and (K,k)
 *
 * @author mark
 */
object SpliceLHHLWeaver
{
	val nbells = 10
	val workingBells = nbells-1
	//val lhg12:Map[Int,String] = Map(1->"a", 2->"b", 4->"c", 5->"d", 7->"e", 8->"f")
	//val lhg10:Map[Int,String] = Map(1->"g", 2->"h", 4->"j", 5->"k", 7->"l", 8->"m")
	val lhg12:Map[Int,String] = Map(1->"a", 2->"b", 3->"C", 4->"c", 5->"d", 6->"D", 7->"e", 8->"f")
	val lhg10:Map[Int,String] = Map(1->"g", 2->"h", 3->"J", 4->"j", 5->"k", 6->"K", 7->"l", 8->"m")
	val lhOrders:Map[String,Int] = (lhg10.toList++lhg12.toList).map(_.swap).toMap

	val lelhRounds = LELH(8,0,lhg10)
	val lelhs = List(lelhRounds, LELH(7,2,lhg12), LELH(6,3,lhg12), LELH(4,4,lhg10), LELH(3,5,lhg10), LELH(2,7,lhg12), LELH(0,8,lhg10))
	val lelhsForward = lelhs.map((x)=>(x.lh,x)).toMap
	val lelhsBackward = lelhs.map((x)=>(x.le,x)).toMap
	val lelhMap:Map[Boolean,Map[Int,LELH]] = Map(true->lelhsForward, false->lelhsBackward)

	val lhGroupsToLEOffset = Map("a"->1, "b"->3, "C"->5, "c"->7, "d"->9, "D"->11, "e"->13, "f"->15, "g"->3, "h"->5, "J"->7, "j"->9, "k"->11, "K"->13, "l"->15, "m"->17)
	val hlsByLEOffset = Map(1->9, 3->1, 5->11, 7->3, 9->13, 11->5, 13->15, 15->7, 17->17)
	def getHLs(lhg: String, coursePos: Int, forward: Boolean):((Int,Int),(Int,Int)) =
	{
		val mod = workingBells*2
		def m(x: Int) = x%mod
		var leOffset = lhGroupsToLEOffset(lhg)
		if (!forward)
			leOffset = m(mod*2-2-leOffset)
		val coursePosOffset = if (forward) coursePos*2 else mod-coursePos*2
		val hl10 = hlsByLEOffset(leOffset) + coursePosOffset
		val hl90 = hl10+workingBells
		( (m(hl10), m(hl10+1)), (m(hl90), m(hl90+1)) )
	}

	val myLHG = "*"
	// Actually the number of distinct LHGs
	val maxMethods = 7

	def main(args: Array[String]): Unit =
	{
		bob10through()
		//bob2through()
	}

	def bob10through() =
	{
		val outputter = new Outputter
		val search2 = new SpliceSearch(1, 2, false, outputter)
		val checkRounds = new CheckRounds(1, search2)
		//val search = new ForcedSpliceSearch(0, 3, true, List("m"), checkRounds)
		//search.doSearch(Set(lelhsForward(0)))
		val search = new SpliceSearch(1, 3, true, checkRounds)
		search.doSearch(Set(), Set())
	}

	def bob2through() =
	{
		val outputter = new Outputter
		val search2 = new SpliceSearch(5, 7, false, outputter)
		val checkRounds = new CheckRounds(6, search2)
		val search = new SpliceSearch(6, 7, true, checkRounds)
		search.doSearch(Set(), Set())
	}

	def mod(n: Int) = (n+workingBells)%workingBells

	def search(current: Int, target: Int, forwards: Boolean, revLHGroups: List[String],
						 leadsIncluded: Set[LELH], hlsIncluded: Set[Int], searcher: Searcher): Unit =
	{
		def next(lhg: String) = searcher.chain((lhg::revLHGroups).reverse, leadsIncluded, hlsIncluded)
		val finish10 = mod(target-current)
		val finish12 = mod(target+1-current)
		lhg10.get(finish10).foreach(next)
		lhg12.get(finish12).foreach(next)

		for (delta <- lhg10.keys)
		{
			val next = mod(current+delta)
			val lelh = lelhMap(forwards).get(next)
			lelh.foreach{(lead) =>
				if (!leadsIncluded(lead))
				{
					val lhg = lead.arrivalGroups(delta)
					if (lhg!=myLHG)
						search(next, target, forwards, lhg::revLHGroups, leadsIncluded + lead, hlsIncluded, searcher)
					else
					{
						val hls = getHLs(lhg, current, forwards)
						val hlSet = if (lhg==myLHG) Set(hls._1._1, hls._1._2) else Set(hls._2._1, hls._2._2)
						val newHLSet = hlsIncluded ++ hlSet
						if (newHLSet.size==hlsIncluded.size+2)
							search(next, target, forwards, lhg :: revLHGroups, leadsIncluded + lead, newHLSet, searcher)
					}
				}
			}
		}
	}

	abstract class Searcher
	{
		var revResults: List[List[String]] = Nil
		def chain(thisResult: List[String], leadsIncluded: Set[LELH], hlsIncluded: Set[Int]): Unit
	}

	class SpliceSearch(start: Int, target: Int, forwards: Boolean, next: Searcher) extends Searcher
	{
		def doSearch(leadsIncluded: Set[LELH], hlsIncluded: Set[Int]) =
		{
			search(start, target, forwards, Nil, leadsIncluded, hlsIncluded, next)
		}

		override def chain(thisResult: List[String], leadsIncluded: Set[LELH], hlsIncluded: Set[Int]) =
		{
			next.revResults = thisResult::revResults
			doSearch(leadsIncluded, hlsIncluded)
		}
	}

	class ForcedSpliceSearch(start: Int, target: Int, forwards: Boolean, lhGroups: List[String], next: Searcher)
			extends SpliceSearch(start, target, forwards, next)
	{
		override def doSearch(leadsIncluded: Set[LELH], hlsIncluded: Set[Int]) =
		{
			search(start, target, forwards, lhGroups.reverse, leadsIncluded, hlsIncluded, next)
		}
	}

	class CheckRounds(start: Int, next: Searcher) extends Searcher
	{
		override def chain(thisResult: List[String], leadsIncluded: Set[LELH], hlsIncluded: Set[Int]) =
		{
			if (leadsIncluded(lelhRounds))
			{
				val (first, last) = splitAtRounds(start, Nil, thisResult)
				if (first.size>=1)
				{
					next.revResults = last :: revResults
					next.chain(first, leadsIncluded, hlsIncluded)
				}
			}
		}
	}

	class Outputter extends Searcher
	{
		override def chain(thisResult: List[String], leadsIncluded: Set[LELH], hlsIncluded: Set[Int]) =
		{
			var results = (thisResult::revResults).reverse
			results = results.tail ++ List(results.head)
			if (results.tail.head.size>=2)
			{
				val flattened = results.flatten
				val nLeads = flattened.size
				val lhGroups = flattened.toSet
				if (nLeads > workingBells - 1 && lhGroups.size == maxMethods)
					if (myLHG=="*" || lhGroups(myLHG))
					{
						//val completion = completeComposition(flattened).mkString(", ")
						val completion = completeCompositionWithSingleLeadEndGroup(flattened).mkString(", ")
						if (completion.nonEmpty)
							println(nLeads + " " + lhGroups.size + " " + results.map(_.mkString).mkString(" / ") + " /  " + completion)
					}
			}
		}
	}

	def splitAtRounds(current: Int, revCoda: List[String], remaining: List[String]): (List[String], List[String]) =
	{
		if (current==0)
			(remaining, revCoda.reverse)
		else
		{
			val lhg = remaining.head
			var next = mod(current + lhOrders(lhg))
			splitAtRounds(next, lhg::revCoda, remaining.tail)
		}
	}
	
	def completeComposition(lhGroups: List[String]): List[String] =
	{
		val completions = ListBuffer[String]()
		val target = Map("a"->7, "b"->7, "C"->7, "c"->7, "d"->7, "D"->7, "e"->7, "f"->7, "g"->6, "h"->6, "J"->6, "j"->6, "k"->6, "K"->6, "l"->6, "m"->6)
		val twoLeadCourses = ListBuffer[List[String]]()
		val threeLeadCourses = ListBuffer[List[String]]()
		val lhGroupSet = lhGroups.toSet
		for (one <- lhGroupSet)
			for (two <- lhGroupSet)
			{
				val offset2 = mod(lhOrders(one)+lhOrders(two))
				if (offset2==target(two))
					twoLeadCourses+= List(one,two)
				for (three <- lhGroupSet)
				{
					val offset3 = mod(offset2+lhOrders(three))
					if (offset3==target(three))
						threeLeadCourses+= List(one,two,three)
				}
			}
		for (two <- twoLeadCourses)
			for (three <- threeLeadCourses)
			{
				val all = lhGroups++two++three
				val balance = all.groupBy(_.toString).mapValues(_.size)
				val min = balance.values.min
				val max = balance.values.max
				val perfect = if (max-min<2) "*" else ""
				val balanceStr = balance.map((p)=> ""+p._2+p._1).mkString(" ")
				if (perfect!="")
					completions+= "("+two.mkString+" "+three.mkString++" "+balanceStr+")"
			}
		completions.toList
	}

	def completeCompositionWithSingleLeadEndGroup(lhGroups: List[String]): List[String] =
	{
		val completions = ListBuffer[String]()
		if (lhGroups.contains("e"))
		{
			val target = Map("a"->7, "b"->7, "C"->7, "c"->7, "d"->7, "D"->7, "e"->7, "f"->7, "g"->6, "h"->6, "J"->6, "j"->6, "k"->6, "K"->6, "l"->6, "m"->6)
			val fourLeadCourses = ListBuffer[List[String]]()
			val lhGroupSet = lhGroups.toSet
			for (one <- lhGroupSet - "e")
				for (two <- lhGroupSet)
				{
					val offset2 = mod(lhOrders(one) + lhOrders(two))
					if (offset2 != target(two))
						for (three <- lhGroupSet)
						{
							val offset3 = mod(offset2 + lhOrders(three))
							if (offset3 != target(three))
								for (four <- lhGroupSet)
								{
									val offset4 = mod(offset3 + lhOrders(four))
									if (offset4 == target(four))
										fourLeadCourses += List(one, two, three, four)
								}
						}
				}
			for (four <- fourLeadCourses)
			{
				val all = lhGroups ++ List("e") ++ four
				val balance = all.groupBy(_.toString).mapValues(_.size)
				val min = balance.values.min
				val max = balance.values.max
				val perfect = if (max - min < 2) "*" else ""
				val balanceStr = balance.map((p) => "" + p._2 + p._1).mkString(" ")
				if (perfect != "")
					completions += "(e " + four.mkString ++ " " + balanceStr + ")"
			}
		}
		completions.toList
	}

	case class LELH(le: Int, lh: Int, arrivalGroups: Map[Int,String])
}