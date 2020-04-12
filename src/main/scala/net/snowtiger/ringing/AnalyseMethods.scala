package net.snowtiger.ringing

import net.snowtiger.methodmaker.MethodSearchStats
import net.snowtiger.spliced.score._

/**
 * @author mark
 */
object AnalyseMethods
{
	def main(args: Array[String]): Unit =
	{
		val nbells = 12
		val methods = /*MethodLibrary.surpriseLibraries.getLibrary(nbells).methods ++ */MethodLibrary.delightLibraries.getLibrary(nbells).methods
		val analyser = AnalyseMethods(methods)
		println("Best course-4: ")
		analyser.printBestCoursingFour(40)
		println
		println("Most interesting: ")
		analyser.printMostInterestingMax(20)
	}
}

case class AnalyseMethods(lib: List[NamedMethod])
{

	def printMostDifficult(n: Int)
	{
		val methodsWithDifficulty = lib.map{(m)=> (m, m.difficulty)}
		val sorted = methodsWithDifficulty.sortBy{-_._2}
		for (m <- sorted.take(n))
			println(m._2+","+MethodSearchStats(m._1).csvStats);
	}

	def printBestCoursingFour(n: Int)
	{
		val methodsWithC4 = lib.map{(m)=> (m, m.coursingFourScore)}
		val sorted = methodsWithC4.sortBy{-_._2}
		for (m <- sorted.take(n))
			println(m._2+","+MethodSearchStats(m._1).csvStats);
	}

	def printMostInterestingMax(n: Int)
	{
		val methods = lib.map{(m)=> (m, m.difficulty + m.coursingFourScore + m.count4Runs + m.count6Runs)}
		val sorted = methods.sortBy{-_._2}
		for (m <- sorted.take(n))
			println(m._2+","+MethodSearchStats(m._1).csvStats);
	}

	def printBestCourses(method: NamedMethod): Unit =
	{
		val music = new CompositeMusic(new MusicRun(4), new Music56Rollup(), new Music65Rollup(), new MusicLB(4), new MusicLB(5), new MusicQueens())
		val bestCourses = method.findBestCourses(Row.genTenorsTogetherCourseHeads(method.nbells), music)
		bestCourses.slice(0,20).foreach(println)
	}

}