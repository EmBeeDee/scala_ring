package net.snowtiger.spliced.atw

import net.snowtiger.spliced.search.coursingorder.CoursingOrder

object AtwUnit
{
	def main(args: Array[String]): Unit =
	{
		// 11-spliced
		val unit = AtwUnit(AtwUnitMethod("24536", "53624", "62453", "53246", "53246", "65324", "65324"), AtwUnitMethod("24653", "52643", "53624", "25463", "25346", "62534", "56234"), AtwUnitMethod("25346", "62534", "56234", "24653", "52643", "53624", "25463"), AtwUnitMethod("25463", "25346", "62534", "56234", "24653", "52643", "53624"), AtwUnitMethod("52643", "65324", "25463", "25346", "62534", "56234", "24536"), AtwUnitMethod("53624", "62453", "53246", "53624", "65324", "24653", "53246"), AtwUnitMethod("56234", "24536", "52643", "65324", "25463", "25346", "62534"), AtwUnitMethod("62453", "53246", "24536", "62453", "62453", "53246", "24653"), AtwUnitMethod("62534", "56234", "24653", "52643", "53624", "25463", "25346"), AtwUnitMethod("65324", "25463", "25346", "62534", "56234", "24536", "52643"), AtwUnitMethod("53246", "24653", "65324", "24536", "24536", "62453", "62453"))
		unit.possibleSplices(false)
	}

	def apply(methods: AtwUnitMethod*): AtwUnit = AtwUnit(methods.toArray)

}

/**
 * A set of methods which are capable of giving ATW with the coursing orders specified for each lead of each method.
 * It is currently assumed that the coursing orders form true and complete courses, i.e. methods A through F
 * (in a 6-method unit) may have different coursing orders in each of their leads (otherwise we don't have spliced!)
 * but that every coursing order is rung to the same leads of the course, and no coursing order is rung to the same
 * lead twice.
 */
case class AtwUnit(methods: Array[AtwUnitMethod])
{
	def nMethods = methods.size

	val allCOs = methods.flatMap{_.cos}.toSet

	/** A map of lead numbers -> method number rung to the given coursing order. */
	def methodLeadsForCO(co: CoursingOrder): Map[Int,Int] =
	{
		/** This is a map in the other direction, from method number to lead numbers rung to the coursing order */
		val leadsPerMethod: Map[Int,Set[Int]]  = methods.zipWithIndex.map{(p)=> (p._2, p._1.leadsForCO(co))}.toMap
		val methodLeads: Map[Int,Int] = leadsPerMethod.flatMap{(p)=> p._2.map{(_,p._1)}}
		methodLeads
	}

	/** For each coursing order, a map of lead numbers -> method number rung to that coursing order in that lead. */
	val methodsPerCO: Map[CoursingOrder, Map[Int, Int]] =
		allCOs.map{(co)=> (co, methodLeadsForCO(co) )}.toMap

	val n = 7

	def possibleSplices(firstOnly: Boolean): List[Array[Char]] =
	{
		var finishQuickly = false
		var solutions = List[Array[Char]]()
		def isTrivialSolution(sol: Array[Char]) = sol.forall(_==sol.head)
		def output(revLHOrders: List[Int], coCourses: Map[CoursingOrder, List[Int]]) =
		{
			if (coCourses.values.forall(_.size==n+1))
			{
				val solution = revLHOrders.reverse.map{ (order) => ('a'+order-1).toChar }.toArray
				if (!isTrivialSolution(solution))
				{
					val lhgIncidence = solution.groupBy{(c)=> c}.mapValues(_.size).toList.sortBy(_._1)
					if (true) //lhgIncidence.size==6)
					{
						solutions = solution :: solutions
						if (firstOnly)
							finishQuickly = true
						else
							println(solution.mkString + " " + lhgIncidence.mkString(" "))
					}
				}
			}
		}
		def nextLead(curr: Int, order: Int) = (curr+order)%n
		def search(revLHOrders: List[Int], coCoursesSoFar: Map[CoursingOrder, List[Int]]): Unit =
		{
			val methodNum = revLHOrders.size
			if (methodNum==nMethods)
				output(revLHOrders, coCoursesSoFar)
			else if (!finishQuickly)
				for (order <- 1 until n)
				{
					val it = coCoursesSoFar.keys.iterator
					var newCoursesSoFar = coCoursesSoFar
					val newRevLHOrders = order::revLHOrders
					val lhOrders = newRevLHOrders.reverse.toArray
					var deadend = false
					while (!deadend && it.hasNext)
					{
						val co = it.next()
						var newCourse = newCoursesSoFar(co)
						val methodsPerLead = methodsPerCO(co)
						if (newCourse.isEmpty)
						{
							// See if this CO course contains this method at all
							methodsPerLead.filterKeys(methodsPerLead(_)==methodNum).toList match
							{
								case Nil => // nope, can't extend this CO course
								case (leadNum,m)::rest => newCourse = nextLead(leadNum,order) :: leadNum :: Nil
							}
						}
						if (newCourse.nonEmpty)
							while (!deadend && methodsPerLead(newCourse.head)<=methodNum && (newCourse.size==1 || newCourse.last!=newCourse.head))
							{
								val newLead = nextLead(newCourse.head, lhOrders(methodsPerLead(newCourse.head)))
								if (newCourse.contains(newLead) && newCourse.last!=newLead)
									deadend = true
								else
									newCourse = newLead :: newCourse
							}
						newCoursesSoFar+= co->newCourse
					}
					if (!deadend)
						search(newRevLHOrders, newCoursesSoFar)
				}
		}
		//println("Non-trivial splices...")
		search(Nil, allCOs.map{(co)=> (co,Nil)}.toMap)
		solutions
	}
}

/** One coursing order for each lead of the method, ordered by lead numbers 0-7 */
case class AtwUnitMethod(coursingOrders: Array[CoursingOrder])
{
	val coLeads: Map[CoursingOrder,Set[Int]] = coursingOrders.map{(co)=> (co, leadsForCO(co))}.toMap
	def leadsForCO(co: CoursingOrder): Set[Int] = coursingOrders.zipWithIndex.filter{_._1==co}.map{_._2}.toSet
	def cos = coLeads.keySet
}

object AtwUnitMethod
{
	def apply(cos: String*): AtwUnitMethod = AtwUnitMethod(cos.map{CoursingOrder(_)}.toArray)
}