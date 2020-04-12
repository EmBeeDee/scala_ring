package net.snowtiger.spliced.atw

import net.snowtiger.ringing.PN
import net.snowtiger.spliced.composition.Stage
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/** The leadheads for one method, represented by a starting lead number in the given ATW course set */
case class MethodCourse(nbells: Int, setNumber: Int, startLead: Int, cos: List[CoursingOrder])
{
	val plainCourseLeads = Stage(nbells).StageLeadheads.map{_.toPerm}.toArray
	val courseheads = cos.map{_.toCourseHead(nbells)}
	val leadNumbers = (startLead until startLead+nbells).map{_ % (nbells-1)}
	val leadheads = courseheads.zip(leadNumbers).map{ (p)=> p._1.apply(plainCourseLeads(p._2)) }
	val courseHead = leadheads.find(_.bellAt(nbells)==nbells).get
	var numberedNodeTable = new MethodNodeTable()

	// Methods below might be more useful if we kept the LHG with the method course. But do some searches vary it?
	def lhPN(lhg: String) = if (lhg.charAt(0)<'g') PN("12") else PN("18")

	def leadends(lhg: String) =
	{
		val plainCourseLeads = new Stage(nbells).StageLeadheads.map{_.toPerm}.toArray
		val lhOrder = CourseStructure.lhGroupToLHOrder(nbells, lhg)
		val lhPerm = plainCourseLeads(lhOrder)
		val lhPn = lhPN(lhg)
		leadheads.map{_.apply(lhPerm).apply(lhPn)}
	}

	lazy val hash = setNumber*1000+startLead
	override def hashCode() = hash

	override def equals(obj: scala.Any) = obj match
	{
		case that: MethodCourse => hash==that.hash
		case _ => false
	}
}