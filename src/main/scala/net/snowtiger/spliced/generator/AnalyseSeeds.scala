package net.snowtiger.spliced.generator

import net.snowtiger.ringing.{Perm, Row}

import scala.io.Source

/**
 * @author mark
 */

object AnalyseSeeds
{
	def perms = Map('H'->Perm("13425"), 'W'->Perm("23145"), 'M'->Perm("12453"), 'B'->Perm("51234"))

	def main(args: Array[String])
	{
		val comps = Source.fromFile(args(0)).getLines().flatMap(parseLine(_))
		val noTriples = comps.filter( consecutiveCallCount(_,3)==0 ).toList
		//val sorted = noTriples.sortBy{ consecutiveCallCount(_,2) }
		val sorted = noTriples.map{(c)=> (nRevisitedCourses(c), c) }.sorted
		for (scoredComp <- sorted)
			println(scoredComp)
		val best = coursingOrders(sorted.head._2)
		def occurs(r: Row) = {val n = best.filter(_==r).size; if (n==1) r.toString else r.toString+" "+n}
		println( best.map{occurs(_)}.mkString("\n") )
	}

	def parseLine(line: String) =
	{
		val split = line.split('\t')
		if (split.size!=4)
			None
		else
			Some(split(3))
	}

	def consecutiveCallCount(calling: String, n: Int) =
	{
		val nospaces = calling.replaceAllLiterally(" ", "")
		val consecs = nospaces.sliding(2).map{(pair)=> if (pair(0)==pair(1) && pair(0)!='B') 1 else 0}.toList
		val counted = consecs.tail.scanLeft(consecs.head){(a,b)=> if (b==0) 0 else a+1}
		if (counted.contains(3))
			println(calling)
		assert(!counted.contains(3))
		counted.filter(_==n-1).size
	}

	def nRevisitedCourses(calling: String) =
	{
		var courses = coursingOrders(calling)
		courses.size - courses.toSet.size
	}

	def coursingOrders(calling: String) =
	{
		var courses = List[Row]()
		var co = Row("53246")
		for (c <- calling)
			if (perms.contains(c))
			{
				co = co.apply(perms(c))
				courses = co::courses
			}
		courses.reverse
	}

}