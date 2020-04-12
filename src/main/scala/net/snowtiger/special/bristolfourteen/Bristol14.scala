package net.snowtiger.bristolfourteen

import scala.collection.mutable

/**
 * Cyclic 13-part
 *
 * @author Mark
 */
object Bristol14
{
	val Bristol = Method("BR", "Bristol", 5, 14*4)
	val LB12 = Method("BT", "Little Bristol12", 3, 12*4)
	val LB = Method("LB", "Little Bristol", 1, 10*4)
	val LP = Method("LP", "Littleport", 12, 8*4)

	val Methods = Map(Bristol.abbrev->Bristol, LB12.abbrev->LB12, LB.abbrev->LB, LP.abbrev->LP)

	val maxLen = 6000/13

	def main(args: Array[String])
	{
		//genTouches(3, 9, 10, 3)
		genTouches(9, 3, 3, 10)
	}

	def outputPeal2(first: Touch, second: Touch, last: Touch)
	{
		val m = first.methods++second.methods++last.methods
		if (m.size>=12 && second.methods.size>1)
			println(m.size+" "+first+" / "+second+" / "+last)
	}
	def outputPeal(first: Touch, second: Touch, last: Touch)
	{
		val len = 13*(first.length+second.length+last.length)
		val m = first.methods++second.methods++last.methods
		if (m.size>=12 && second.methods.size>1)
		//if (len<6000 && second.methods.size>1)
		{
			println("   0")
			first.output
			println("-----")
			second.output
			println("-----")
			last.output
			println("len = "+len)
			println()
		}
	}

	def genTouches(t1End: Int, t2Start: Int, t2End: Int, t3Start: Int)
	{
		val first = genLeads(0, t1End, maxLen*3/4)
		val second = genLeads(t2End, t2Start, maxLen)
		val last = genLeads(t3Start, 0, maxLen*3/4)

		if (false)
		{
			outputTouches("=== First", first)
			outputTouches("=== Second", second)
			outputTouches("=== Last", last)
			println
		}

		println("=== Peals")

		for (t1<-first)
		{
			for (t2<-second)
			{
				if (t1.leads.intersect(t2.leads).isEmpty)
				{
					val leads = t1.leads++t2.leads
					for (t3<-last)
					{
						val uniqueLastLeads = if (t1End==t3Start) t3.leads-t1End-0 else t3.leads-0
						if (leads.intersect(uniqueLastLeads).isEmpty)
							outputPeal(t1, t2.reverse, t3)
					}
				}
			}
		}
	}

	def outputTouches(title: String, touches: Set[Touch])
	{
		println(title)
		for (touch <- touches)
		{
			//touch.output
			println(touch.length+" "+touch.methods.map(_.abbrev)+" "+touch.leads)
			//println
		}
	}

	def genLeads(from: Int, to: Int, maxLen: Int) =
	{
		val touches: mutable.Set[Touch] = mutable.Set()

		def gen(len: Int, curLead: Int, leads: Set[Int], methods: List[Method])
		{
			for (method <- Methods.values)
			{
				val newLen = len + method.length
				val newLead = (curLead + method.lead)%13
				if (newLead==to && newLen<=maxLen)
				{
					val touch = new Touch(newLen, from, leads+newLead, (method::methods).reverse)
					touches+= touch
				}
				else if (newLen<maxLen && !leads.contains(newLead))
				{
					gen(newLen, newLead, leads+newLead, method::methods)
				}
			}
		}

		gen(0, from, Set(from), Nil)
		touches.toSet
	}

	case class Touch(length: Int, from: Int, leads: Set[Int], methods: List[Method], reversed: Boolean)
	{
		def this(length: Int, from: Int, leads: Set[Int], methods: List[Method]) = this(length, from, leads, methods, false)

		def output
		{
			var lead = from
			for (method <- methods)
			{
				if (reversed)
					lead = (lead+13-method.lead)%13
				else
					lead = (lead+method.lead)%13
				println(method.abbrev+" "+lead)
			}
		}

		def reverse =
		{
			var lead = from
			for (method <- methods)
				lead = (lead+method.lead)%13
			new Touch(length, lead, leads, methods.reverse, true)
		}

		override def toString = methods.map(_.abbrev).mkString(" ")
	}

	case class Method(abbrev: String, name: String, lead: Int, length: Int)
}
