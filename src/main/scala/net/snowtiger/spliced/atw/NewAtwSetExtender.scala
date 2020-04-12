package net.snowtiger.spliced.atw

import java.io.PrintStream

import net.snowtiger.spliced.atw.NewAtwGroupFinder.{allCourses, nbells, plainCourseLeads, protoMethod}
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * @author mark
 */

object NewAtwSetExtender
{
	val allowedCOs = CoursingOrder.positive.toSet

	val MaxCourses = 11
	val MinMethods = MaxCourses
	val MaxSameCO = 4
	val MaxDiff = 0
	val MaxCOs = MaxCourses

	val positiveCourseSets = AtwGroupFinder2.findATW(8)

	var unitsCOsFound = Set[Set[CoursingOrder]]()

	def main(args: Array[String]): Unit =
	{
		val start = CoursingOrder.Start
		val unit = new MethodUnit(List(Method(start)))
		val t = System.currentTimeMillis()
		recursiveBuild(unit, Set(start))
		val dur = (System.currentTimeMillis() - t)/1000
		println("Time taken: "+dur+"s")
	}

	def recursiveBuild(unit: MethodUnit, cosUsed: Set[CoursingOrder]): Boolean =
	{
		var deadend = true
		if (unit.courses.size<=MaxCourses)
		{
			val allowed = if (cosUsed.size==MaxCOs) cosUsed else allowedCOs
			for (co <- unit.availableCOs(allowed))
				recursiveBuild(unit.add(co), cosUsed+co)
			if (unit.courses.head.complete && deadend)
			{
				if (unit.isGood)
				{
					val splices = AtwUnit(unit.courses.map{_.toAtwUnitMethod}.toArray).possibleSplices(true)
					if (splices.size>0)
					{
						val cosVisited = unit.cosVisited.toSet
						if (!unitsCOsFound.contains(cosVisited))
						{
							for (splice <- splices)
							{
								val lhgIncidence = splice.groupBy{(c)=> c}.mapValues(_.size).toList.sortBy(_._1)
								println(splice.mkString + " " + lhgIncidence.mkString(" "))
							}
							unit.output(System.out)
							unitsCOsFound+= cosVisited
							println("Combined...")
							AtwGroupFinder2.combineCOSets(positiveCourseSets, 3, Some(cosVisited))
							println()
						}
					}
				}
				deadend = false
			}
		}
		deadend
	}

	val callingPositions = List(0,1,3,6)
	//val callingPositions = List(0,1,6)

	abstract class Method(val cos: Array[CoursingOrder])
	{
		val leadNum = cos.size
		val cosAtMax: Set[CoursingOrder] = cos.toSet.map{(co: CoursingOrder)=> (co, cos.count(_==co))}.toMap.filter{(p)=> p._2>=MaxSameCO}.keySet

		def size = leadNum
		def complete = cos.size==nbells-1

		def lead(leadNum: Int) = cos(leadNum)
		def coExistsAtCallingPosition(co: CoursingOrder) = callingPositions.exists{cos(_)==co}

		def availableCOs(allCOs: Set[CoursingOrder]): Set[CoursingOrder]
		def add(newCO: CoursingOrder): Method

		def toAtwUnitMethod = AtwUnitMethod(cos)
	}

	object Method
	{
		/** Map of leadNum -> list of places in the lead (0-6) where 53246 fall */
		val leadCOPositions = plainCourseLeads.zipWithIndex.map{(p)=> (p._2, List(5,3,2,4,6).map{p._1.placeOf(_)-2}) }.toMap
		val allFrontBells = "23456".toSet
		def apply(courseHeadCO: CoursingOrder) = newImpl2(courseHeadCO)

		def newImpl1(courseHeadCO: CoursingOrder) = new AtwMethodImpl(Array(courseHeadCO), new SingleMethodAtw(protoMethod)+allCourses(courseHeadCO)(0))
		def newImpl2(courseHeadCO: CoursingOrder) = new AtwMethodImpl2(new Array(0), Array.fill(7)(Method.allFrontBells)).add(courseHeadCO)
	}

	class AtwMethodImpl(cos0: Array[CoursingOrder], singleMethodAtw: SingleMethodAtw) extends Method(cos0)
	{
		override def availableCOs(allCOs: Set[CoursingOrder]) =
		{
			val diffCOs = allCOs.filter{!cosAtMax(_)}
			val available = diffCOs.filter{newAtw(_).score==(leadNum+1)*(nbells-1)}
			available
		}

		protected def newAtw(co: CoursingOrder) = singleMethodAtw + allCourses(co)(leadNum)

		override def add(newCO: CoursingOrder) = new AtwMethodImpl(cos:+newCO, newAtw(newCO))
	}

	/** bellsUnused in lead-row order, i.e. array is always of length 7, however the tenors are never present */
	class AtwMethodImpl2(cos0: Array[CoursingOrder], bellsUnused: Array[Set[Char]]) extends Method(cos0)
	{
		override def availableCOs(allCOs: Set[CoursingOrder]) =
		{
			val unusedForThisCO = Method.leadCOPositions(leadNum).map{bellsUnused(_)}.toArray
			val checkOrder = unusedForThisCO.zipWithIndex.sortBy{_._1.size}.map{_._2}
			var available = allCOs.filter{!cosAtMax(_)}
			for (i <- checkOrder)
				available = available.filter{(co)=> unusedForThisCO(i).contains(co.raw.charAt(i))}
			available
		}

		override def add(newCO: CoursingOrder) =
		{
			val newUnused = bellsUnused.clone()
			val positions = Method.leadCOPositions(leadNum)
			for ( (bell, pos) <- newCO.raw.zip(positions))
				newUnused(pos)-= bell
			new AtwMethodImpl2(cos:+newCO, newUnused)
		}
	}

	case class MethodUnit(courses: List[Method])
	{
		def isComplete = courses.isEmpty || courses.head.size==nbells-1

		def availableCOs(allCOs: Set[CoursingOrder]): Set[CoursingOrder] =
		{
			if (courses.isEmpty || courses.head.size==nbells-1)
			{
				// Starting a new method - the main criteria is that the CO must be different from previous courses
				// However in order to avoid generating duplicate solutions, the CO must also be "less than" the previous course
				val trueCOs = allCOs.filter{ (co) => !courses.exists(_.cos(0) == co) }
				if (courses.size<2)
					trueCOs
				else
					trueCOs.filter{_.raw<courses.head.cos(0).raw}
			}
			else
			{
				// Adding to an existing method - the CO must be different from previous courses, and must also be ATW
				// with respect to previous leads of the same method.
				val leadNum = courses.head.size
				val trueCOs = allCOs.filter{(co)=> !courses.tail.exists(_.cos(leadNum)==co)}
				courses.head.availableCOs(trueCOs)
			}
		}

		def add(newCO: CoursingOrder) =
		{
			val newCourses =
				if (isComplete)
					Method(newCO)::courses
				else
					courses.head.add(newCO)::courses.tail
			new MethodUnit(newCourses)
		}

		lazy val cosVisited: List[CoursingOrder] = courses.toSet.flatMap{(m: Method)=> m.cos.toSet}.toList

		def output(out: PrintStream) =
		{
			val completeCourses = courses.filter(_.complete).reverse
			val nMethods = completeCourses.size
			val nCOs = cosVisited.size
			val diff = nCOs-nMethods
			println("=============================================")
			println("Methods = "+nMethods+" COs = "+nCOs+" Diff = "+diff)
			for (leadNum <- 0 until nbells-1)
			{
				val cosAtThisLead = completeCourses.map{_.lead(leadNum)}.toSet
				def coVisited(co: CoursingOrder) = if (cosAtThisLead(co)) co else "     "
				out.println(plainCourseLeads(leadNum).maskFrontBellsNotTreble(6) + " " +
						completeCourses.map{_.lead(leadNum)}.mkString(" ") + "   " +
						cosVisited.map(coVisited).mkString(" "))
			}
			println(courses.map{_.cos.mkString("AtwUnitMethod(\"", "\", \"", "\")")}.mkString("AtwUnit(", ", ", ")"))
			println("=============================================")
		}

		def isGood =
		{
			val completeCourses = courses.filter(_.complete).reverse
			val nMethods = completeCourses.size
			val nCOs = cosVisited.size
			val diff = nCOs-nMethods
	    nMethods>=MinMethods && diff<=MaxDiff && cosVisited.forall{(co)=> completeCourses.exists(_.coExistsAtCallingPosition(co))}
		}
	}
}