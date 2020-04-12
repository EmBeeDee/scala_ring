package net.snowtiger.special.stedmancinques

import net.snowtiger.ringing.{PN, Perm, Row}
import net.snowtiger.special.stedmancinques.StedmanCourse._

import scala.collection.mutable

/**
 * @author mark
 */
object StedmanCourse
{
	val positions = List(5, 6, 7, 9, 14, 16, 18, 19)
	val perms = Map(5->MainPos("425631"), 6->MainPos("135264"), 7->MainPos("315264"), 9->SinglePos("163452"), 14->SinglePos("124356"),
		16->SinglePos("126453"), 18->MainPos("436512"), 19->MainPos("235164"))
	val postCourseEndPns = PN.parse("E.3.1.3.1.3.E.1.3.1.3.1", 11)
	val preCourseEndPns = PN.parse("1.3.1.3.1.E.3.1.3.1.3", 11)
	val back4Set = Set("1234", "2143", "2345", "3254", "3456", "4365", "4321", "3412", "5432", "4523", "6543", "5634")
	val back5Set = Set("12345", "23456", "54321", "65432")
	val front5Set = back5Set++"34567"

	val courseCache = mutable.Map[(Row,String),StedmanCourse]()

	def apply(ch: String, calling: String) = new StedmanCourse(Row(ch), calling)

	def ifMusical(ch: Row, calling: String): Option[StedmanCourse] =
	{
		val course = courseCache.getOrElseUpdate(Tuple2(ch, calling), StedmanCourse(ch, calling))
		if (course.nMusic>0)
			Some(course)
		else
			None
	}

	trait Calls
	{
		val perms:Map[Char,Perm]
		def perm(row: Row, call: Char): Row = row.apply(perms(call))
	}

	case class MainPos(plainStr: String) extends Calls
	{
		assert(plainStr.length==6)
		val plain = Perm(plainStr)
		val perms = Map('P'->plain, 'B'->Perm("126435").permuteBy(plain), 'S'->Perm("126453").permuteBy(plain))
	}

	case class SinglePos(singleStr: String) extends Calls
	{
		val perms = Map('P'->Perm("123456"), 'S'->Perm(singleStr))
	}
}

case class StedmanCourse(courseHead: Row, calling: String)
{
	val (courseEnd, music, truthRows) = parseCalling
	val (frontMusic, backMusic) = music.partition{_.startsWith("f")}
	val nMusic = backMusic.size
	/** This counts 65 course ends if sixthHomePos==5 */
	val score56 = if (courseHead.bellAt(if (LBStedman.sixthHomePos==5) 6 else 5)==5) 1 else 0
	val scoreBackLB5 = backMusic.count{_.length>4}
	val musicScore = nMusic*3 + frontMusic.size + score56 + scoreBackLB5
	val sortPreference = -100*musicScore + calling.length

	override def toString = courseHead+" -> "+courseEnd+" ("+musicScore+") "+calling+" "+music.mkString("(",", ",")")

	def trueWith(other: Set[String]) = truthRows.forall(!other.contains(_))

	/** Gives course end, list of music, strings for truth-checking */
	def parseCalling: (Row,List[String],Set[String]) =
	{
		var calls = calling.split(' ').toList
		var music = List[String]()
		var truthStrings = Set[String]()

		val longCourseHead = Row(courseHead.toString+"8709E")
		var openingRows = PN.generateRows(longCourseHead, postCourseEndPns)
		music++= openingRows.flatMap(isFrontRun)
		truthStrings++= openingRows.map{_.toString}.grouped(6).map{_.max}

		var row = courseHead
		var posChar = 'a'
		for (callPos <- positions)
		{
			var call = 'P'
			if (calls.nonEmpty)
			{
				if (calls.head==""+callPos)
				{
					call = 'B'
					calls = calls.tail
				}
				else if (calls.head=="s"+callPos)
				{
					call = 'S'
					calls = calls.tail
				}
			}
			row = perms(callPos).perm(row, call)
			extractMusic(callPos, row) match
			{
				case Some(m) => music = m::music
				case None => // no-op
			}
			truthStrings+= ""+posChar+row
			posChar = (posChar+1).toChar
			//println(row+" "+call)
		}
		val courseEnd = row.apply(Perm("123546"))
		//println(courseEnd)

		val longCourseEnd = Row(courseEnd.toString+"8709E")
		var closingRows = longCourseEnd::PN.generateRows(longCourseEnd, preCourseEndPns)
		music++= closingRows.flatMap(isFrontRun)
		truthStrings++= closingRows.map{_.toString}.grouped(6).map{_.max}

		(courseEnd, music, truthStrings)
	}

	def extractMusic(pos: Int, row: Row): Option[String] =
	{
		if (pos==5 || pos==6 || pos==18)
		{
			val back4 = row.toString.substring(2)
			val back5 = getBack5(pos, row)
			if (is5run(back5))
				Some(back5)
			else if (is4run(back4))
				Some(back4)
			else
				None
		}
		else
			None
	}

	def getBack5(pos: Int, row: Row): String =
	{
		val rowStr = row.toString
		if (pos==5)
			rowStr.substring(1)
		else if (pos==6)
			""+rowStr(1)+rowStr(3)+rowStr(2)+rowStr(5)+rowStr(4)
		else
			""
	}

	def is4run(back4: String) = back4Set.contains(back4)

	def is5run(back5: String) = back5Set.contains(back5)

	def isFrontRun(row: Row): Option[String] =
	{
		val rowStr = row.toString
		if (rowStr.startsWith("123546") || front5Set.contains(rowStr.substring(0,5)))
			Some("f"+rowStr.substring(0,5))
		else
			None
	}

}

