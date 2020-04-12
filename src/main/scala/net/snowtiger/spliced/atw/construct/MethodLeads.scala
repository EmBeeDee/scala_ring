package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{Method, PN, Perm, Row}
import net.snowtiger.spliced.atw.{CourseStructure, MethodCourse, MultiBitSet}
import net.snowtiger.spliced.composition.Stage

/** The set of leadheads and leadends for one method in the composition, plus all possible halfleads for the first lead. */
case class MethodLeads(course: MethodCourse, lhGroup: String)
{
	import MethodLeads._
	val lePerm = lePerms(lhGroup)
	// The first two rows will be the leadhead and leadend of the first lead in the course-set
	val rows = buildLeads
	val halfleads = buildHalfleads
	val pos = ""+course.setNumber+"."+course.startLead

	def genAllRows(pns: List[PN]): MultiBitSet =
	{
		var rs = rows
	  var mbs = MultiBitSet(rows)
		for (pn <- pns)
		{
			rs = rs.map{_.apply(pn)}
			mbs = mbs + MultiBitSet(rs)
		}
		mbs
	}

	def leadends = course.leadends(lhGroup)

	private def buildLeads: List[Row] =
	{
		var revRows = List[Row]()
		for ( lh <- course.leadheads)
		{
			revRows = lh::revRows
			revRows = lh.apply(lePerm)::revRows
		}
		revRows.reverse
	}
	private def buildHalfleads: Map[Row,PN] =
	{
		val leadendFromRounds = rounds.apply(lePerm)
		val fixedBells = leadendFromRounds.bellsInSamePlace(rounds).toList
		def halfLeadPN(halflead: Row) = PN(fixedBells.map{halflead.placeOf(_)-1}.sorted)
		val validHalfLeadsFromRounds = Method.genAllHalfLeadsForLeadEnd(leadendFromRounds) //.filter{_.isPlainBob}
		val halfLeadPNs = validHalfLeadsFromRounds.map{halfLeadPN}
		val validHalfLeads = validHalfLeadsFromRounds.map{(r:Row)=> r.permuteBy(rows(0).toPerm)}
		validHalfLeads.zip(halfLeadPNs).toMap
	}
}

object MethodLeads
{
	val nbells = 8
	val rounds = Row(8)
	val plainCourseLeads = new Stage(nbells).StageLeadheads.map{_.toPerm}.toArray
	val pn12 = PN("12")
	val pn18 = PN("18")
	val allLHGroups = List("a","b","c","d","e","f", "g","h","j","k","l","m")
	val lhPerms = allLHGroups.zip(CourseStructure.lhGroupsToLHOrders(nbells, allLHGroups).map{plainCourseLeads(_)})
	val (lhPerms12,lhPerms18) = lhPerms.splitAt(6)
	def leadendPerm(p: (String,Perm), lhPN: PN) = (p._1, rounds.apply(p._2).apply(lhPN).toPerm)
	val lePerms = (lhPerms12.map{leadendPerm(_,pn12)} ++ lhPerms18.map{leadendPerm(_,pn18)}).toMap
}