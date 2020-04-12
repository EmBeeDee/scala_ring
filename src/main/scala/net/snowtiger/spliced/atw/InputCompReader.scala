package net.snowtiger.spliced.atw

import scala.io.Source

/**
 * Reads input comps and course sets as output from {@link MatchCallingToSplices}.
 * @author mark
 */
class InputCompReader(nbells: Int)
{
	val UnrotatedCallingText = "Unrotated calling = "
	val ScoreText = "Score: "
	val RotateFromText = "(from "

	def read(file: String, minRequiredCourseSetSize: Int) =
	{
		val lines = Source.fromFile(file).getLines()
		var revComps = List[InputComp]()

		while (lines.hasNext)
		{
			val line = lines.next()
			// Requires blank line before matching start of next comp
			if (line.startsWith(UnrotatedCallingText))
				parseComp(line.substring(UnrotatedCallingText.length), lines, minRequiredCourseSetSize) match
				{
					case Some(comp) => revComps = comp::revComps
					case None => // no-op
				}
		}
		assert(!revComps.isEmpty)
		val nCourseSets = revComps.head.courses.size
		// All comps are expected to have the same number of course sets
		assert(revComps.forall{_.courses.size==nCourseSets})
		revComps.reverse
	}

	def parseComp(calling: String, lines: Iterator[String], minRequiredCourseSetSize: Int): Option[InputComp] =
	{
		val scoreLine = lines.next()
		var i = scoreLine.indexOf(ScoreText)
		var j = scoreLine.indexOf(RotateFromText)
		if (i<0 || j<i)
			None
		else
		{
			i+= ScoreText.length
			val score = scoreLine.substring(i, j).trim.toInt
			j+= RotateFromText.length
			val rotateTo = scoreLine.substring(j, j+5)
			var revCourses = List[InputCourse]()
			var coursesLeft = lines.hasNext
			while (coursesLeft)
				parseCourseSet(lines.next()) match
				{
					case Some(inputCourse) => revCourses = inputCourse::revCourses; coursesLeft = lines.hasNext
					case None => coursesLeft = false
				}
			if (revCourses.exists{(c)=> c.shortestSplice<=minRequiredCourseSetSize})
				Some(InputComp(nbells, calling, score, rotateTo, revCourses.reverse))
			else
				None
		}
	}

	def parseCourseSet(line: String): Option[InputCourse] =
	{
		val inputCourseText = "InputCourse("
		val i = line.indexOf(inputCourseText)
		if (i<=0)
			None
		else
		{
			val split = line.substring(i).split('"')
			val cos = split(1)
			val splices = split(3)
			Some(InputCourse(nbells, cos, splices))
		}
	}
}