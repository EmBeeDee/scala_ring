package net.snowtiger.spliced.atw

import net.snowtiger.ringing.NamedMethod

import scala.io.Source

/**
 * Reads lists of popular methods per course set, as output by {@link AtwMethodFinder3} in single-course-set mode.
 *
 * @author mark
 */
class MethodPopularityReader(val libraryMethods: List[NamedMethod])
{
	val allMethods = libraryMethods.groupBy{(m)=> m.name+" ("+m.lhGroup+")"}

	var methodsPerComp: Array[Array[List[NamedMethod]]] = _

	def readAll(nBest: Int)
	{
		val set1 = read("popular_set1.txt")
		val set2 = read("popular_set2.txt")
		val set3 = read("popular_set3.txt")
		val set4 = read("popular_set4.txt")
		val nComps = set1.size
		methodsPerComp = new Array[Array[List[NamedMethod]]](nComps)
		for (i <- 0 until nComps)
		{
			val compSet1 = set1(i).take(nBest)
			val compSet2 = set2(i).take(nBest)
			val compSet3 = set3(i).take(nBest)
			val compSet4 = set4(i).take(nBest)
			val compSets = Array(compSet1, compSet2, compSet3, compSet4)
			methodsPerComp(i) = compSets
		}
	}

	def read(file: String) =
	{
		println("Reading popularity set: "+file)
		val lines = Source.fromFile(file).getLines()
		var methodLists = List[List[NamedMethod]]()

		while (lines.hasNext)
		{
			val line = lines.next()
			if (line.startsWith("Most popular"))
				methodLists = parseMethodList(lines)::methodLists
		}
		methodLists.reverse.toArray
	}

	def parseMethodList(lines: Iterator[String]) =
	{
		var methods = List[NamedMethod]()
		var finished = false
		while (lines.hasNext && !finished)
		{
			val line = lines.next()
			if (line.isEmpty || !line(0).isDigit)
				finished = true
			else
			{
				val name = line.substring(line.indexOf(' ')+1)
				methods++= allMethods(name)
			}
		}
		methods
	}
}