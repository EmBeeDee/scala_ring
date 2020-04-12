package net.snowtiger.special.stedmancinques

import net.snowtiger.ringing.{Music, Perm, Row}
import net.snowtiger.spliced.score.{Music4Course, Music56Rollup, MusicQueens, MusicRun}

/**
 * Finds Stedman Cinques turning courses, between a given start and finish row, up to a specified number of sixes.
 * Note the start and end are always taken to be the final row of a quick six, i.e. the start is E (or 9/90E if there's a call).
 *
 * @author mark
 */
object TurningCourseFinder
{
	val PP = ("PP", Perm("2468103E597"))
	val PB = ("PB", Perm("246810375E9"))
	val PS = ("PS", Perm("2468103759E"))
	val BP = ("BP", Perm("246819305E7"))
	val BB = ("BB", Perm("2468193750E"))
	val BS = ("BS", Perm("246819375E0"))
	val SP = ("SP", Perm("2468193E507"))

	val twelves = List(PP, PB, PS, BP, BB, BS, SP)
	val plainStarts = twelves.take(3)

	val slowP = Perm("241638507E9")
	val slowB = Perm("2416385970E")
	val slowS = Perm("241638597E0")
	val slowPerms = Map('P'->slowP, 'B'->slowB, 'S'->slowS)

	val sixPerms = List(Perm("1234567890E"),Perm("132547698E0"),Perm("3124567890E"),Perm("321547698E0"),Perm("2314567890E"),Perm("213547698E0"))
	val twelvePerms = twelves.toMap

	val MIN_QUEENS = 1

	def main(args: Array[String]): Unit =
	{
		//search(Row("****658709E"), Row("****658709E"), 20)
		//search(Row("****567890E"), Row("****6587E90"), 20)
		//search(Row("****6587E90"), Row("****658709E"), 18)
		//search(Row("14326587E90"), Row("1432658709E"), 20)

		search(Row("****567809E"), Row("****567809E"), 22)
		//search(Row("****6587E90"), Row("****6587E90"), 22)
	}

	def search(start: Row, finish: Row, maxSixes: Int): Unit =
	{
		println("Searches for turning courses up to "+maxSixes+" sixes long, starting "+start+" and finishing "+finish)
		search2("", start, maxSixes, None, SearchParams(start, finish))
	}

	def search1(comp: String, current: Row, sixesLeft: Int, slowChain: Option[List[Int]], params: SearchParams): Unit =
	{
		if (current==params.finish)
			output(params.start, comp)
		else if (sixesLeft>0)
			search2(comp, current, sixesLeft, slowChain, params)
	}

	def search2(comp: String, current: Row, sixesLeft: Int, slowChain: Option[List[Int]], params: SearchParams): Unit =
	{
		// The slow chain is an important pruning optimisation - without it we could not search beyond about 16 sixes.
		// The chain starts with the bell in the finishing row which has been unaffected by calls for the longest time.
		// In Cinques, this is the bell in 8-9 up at the finish; it came out slow and so has been unaffected for 10 sixes.
		// The start of the slow chain is therefore a twelve-end with this bell in 9-8 down, ready to go in slow.
		// Once a twelve-end of this nature is found, and there is no longer time for that bell to return to the chain start,
		// the sequence of slow bells is then fixed, i.e. we can terminate the search if the next slow bell doesn't appear
		// in the right place.
		var slowChainOK = true
		var newSlowChain: Option[List[Int]] = None
		if (slowChain.isEmpty)
		{
			newSlowChain = params.startSlowChain(current, sixesLeft)
			if (newSlowChain.isEmpty && sixesLeft <= params.slowChainLength)
				slowChainOK = false
		}
		else
		{
			if (params.continueSlowChain(current, slowChain.get))
				newSlowChain = Some(slowChain.get.tail)
			else
				slowChainOK = false
		}
		if (slowChainOK)
		// Protocol is to allow singles only at the end of a sequence of calls, i.e. -S not S-
		// Note this can affect music
			for ((s, perm) <- (if (comp.endsWith("S")) plainStarts else twelves))
				search1(comp + s, current.apply(perm), sixesLeft-2, newSlowChain, params)
	}

	val music56 = new Music56Rollup()
	val musicCourse = new Music4Course()
	val music4run = new MusicRun(4)
	val musicQueens = new MusicQueens()


	def output(start: Row, comp: String): Unit =
	{
		// Convert sequence of PBS to calling positions e.g. 1 s14
		def convert2(call: Char, pos: Int) = (if (call=='S') "s" else "")+(if (call!='P') (pos+1).toString+" " else "")
		def convert1(call: (Char,Int)) = convert2(call._1, call._2)
		var byCallingPos = comp.zipWithIndex.map{convert1}.mkString
		// Recover all rows and count music. TODO music may change with singles moved/added within sequences of calls.
		val allRows = recoverRows(start, comp)
		def musStr(music: Music, name: String) = " "+name+"="+music.countMusic(allRows)
		val music = musStr(music56, "56") + musStr(musicCourse, "4course") + musStr(music4run, "4run") + musStr(musicQueens, "queens")
		val len = comp.length
		if (musicQueens.countMusic(allRows)>=MIN_QUEENS)
			println(comp + music + "  " + byCallingPos + "("+len+")")
	}

	def recoverRows(start: Row, comp: String): List[Row] =
	{
		var current = start
		def recoverSix(sixEnd: Row): List[Row] = sixPerms.map{sixEnd.apply}
		def recoverRows(callPair: String): List[Row] =
		{
			val firstSixEnd = current.apply(slowPerms(callPair(0)))
			current = current.apply(twelvePerms(callPair))
			recoverSix(firstSixEnd) ++ recoverSix(current)
		}
		comp.grouped(2).toList.flatMap(recoverRows)
	}

	case class SearchParams(start: Row, finish: Row)
	{
		// Bell in 8th's place goes into the slow in Cinques
		val slowChainStartPos = 8
		// Bells which have to go in slow, starting 10 sixes from the end
		val slowBellChain = List(9, 5, 1, 2, 4).map(finish.bellAt)
		val slowChainLength = slowBellChain.length*2
		// Takes at minimum 14 changes to go from one slow-bell start pos to the next (with single bob at back),
		// so can't start the slow chain earlier than this many sixes from the end, plus the length of the slow chain itself.
		val slowChainEarliestStart = 14+slowChainLength

		def startSlowChain(current: Row, sixesLeft: Int): Option[List[Int]] =
		{
			if (sixesLeft<slowChainEarliestStart)
			{
				val slowBell = current.bellAt(slowChainStartPos)
				if (slowBell==slowBellChain.head)
					Some(slowBellChain.tail)
				else
					None
			}
			else
				None
		}

		def continueSlowChain(current: Row, remainingChain: List[Int]): Boolean =
		{
			remainingChain.nonEmpty && current.bellAt(slowChainStartPos)==remainingChain.head
		}
	}
}