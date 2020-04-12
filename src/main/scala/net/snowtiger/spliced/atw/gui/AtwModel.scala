package net.snowtiger.spliced.atw.gui

import net.snowtiger.ringing._
import net.snowtiger.spliced.atw.AtwMethodFinder.StandardMethodProvider
import net.snowtiger.spliced.atw._
import net.snowtiger.spliced.atw.construct.{MethodLeads, SectionTables}

import scala.collection.mutable.ListBuffer

/**
 * @author mark
 */

class AtwModel
{
	val atwMethodFinder = AtwMethodFinder
	val nbells = atwMethodFinder.nbells
	val rounds = Row(nbells)
	val methodProvider = new StandardMethodProvider(atwMethodFinder.searchMethods)

	private val rawComp = atwMethodFinder.comps(2)
	private val shortestSpliceComp = rawComp.preferShortest
	private val longestSpliceComp = rawComp.preferLongest
	val comp = new InputComp(rawComp, longestSpliceComp.courses.slice(0,2)++shortestSpliceComp.courses.slice(2,4))

	// Order is tricky... output from other searches is in leadNum not splice order!
	private val lhGroups: List[String] = "b,d,e,d,b,d, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".filter{_!=' '}.split(',').toList
	//private val lhGroups: List[String] = "f,l,f,f,f,f, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".filter{_!=' '}.split(',').toList
	//private val lhGroups: List[String] = "a,j,f,f,d,a, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".filter{_!=' '}.split(',').toList

	private val courses = genMethodLeads(comp, lhGroups)
	val nMethods = courses.size
	println("Generating "+nMethods+" MethodRows")
	private val methods = courses.zipWithIndex.map{(p)=> new MethodRows(p._1, p._2)}.toArray
	println("Done")

	def modelData(methodNum: Int) = methods(methodNum)

	private def genMethodLeads(comp: InputComp, lhGroups: List[String]): List[MethodLeads] =
	{
		val lhGroupsPerCourse = comp.unflattenByCourse(lhGroups)
		val cos = comp.getCompCOs
		val methodCourses = lhGroupsPerCourse.zip(cos).zipWithIndex.flatMap{(p)=> atwMethodFinder.genMethodCourses(p._2, p._1._1, p._1._2)}
		var revResults = List[MethodLeads]()
		for ( (lhGroup, course) <- lhGroups.zip(methodCourses))
			revResults = MethodLeads(course, lhGroup)::revResults
		revResults.reverse
	}

	/** Current state of one method slot in the composition, with filled-in rows and choices for remaining rows */
	class MethodRows(leads: MethodLeads, val methodNum: Int)
	{
		val lhGroup = lhGroups(methodNum)
		var allRows = MultiBitSet(leads.rows)
		val firstLH = leads.rows.head

		val displayLeadLen = nbells*2
		val displayRows = Array.fill[Option[Row]](displayLeadLen)(None)
		displayRows(0) = Some(firstLH)

		def name = method match {case None => "Method "+(methodNum+1); case Some(nm) => nm.name}
		var pnList = List[PN]()
		var method: Option[NamedMethod] = None

		val possibleMethods: List[(NamedMethod,MultiBitSet)] = methodProvider.allMethods(lhGroup).flatMap(methodRows)

		def isFinished = nRowsFinished==displayLeadLen
		def nRowsFinished = displayRows.takeWhile{_.isDefined}.size

		def set(newMethod: NamedMethod)
		{
			method = Some(newMethod)
			pnList = newMethod.lead.slice(0,displayLeadLen-1).toList
			val buf = ListBuffer[Row]()
			buf+= PN.generateChanges(firstLH, pnList, buf)
			buf.toList.zipWithIndex.foreach{(p)=> displayRows(p._2) = Some(p._1)}
			addHalfleadPN()
			genAllRows()
		}

		def set(rowIndex: Int, pn: PN)
		{
			pnList = pnList.slice(0,rowIndex) ++ List(pn)
			displayRows(rowIndex+1) = Some(displayRows(rowIndex).get.apply(pn))
			reset(rowIndex+1)
		}

		def reset(rowIndex: Int)
		{
			for (j <- rowIndex+1 until displayLeadLen)
				displayRows(j) = None
			pnList = pnList.slice(0,rowIndex)
			method = None
			addHalfleadPN()
			genAllRows()
		}

		def addHalfleadPN()
		{
			displayRows(displayLeadLen-1) match
			{
				case Some(row) => leads.halfleads.get(row) match {
					case Some(pn) => pnList = pnList.slice(0,displayLeadLen-1)++List(pn)
					case None => // no-op, but shouldn't happen!
				}
				case None => // no-op
			}
		}

		def methodChoices(rowIndex: Int) =
		{
			val withCorrectPrefix = possibleMethods.filter{_._1.lead.slice(0,rowIndex)==pnList.slice(0,rowIndex)}
			val andTrue = withCorrectPrefix.filter{(p)=> trueAgainstOthers(p._2)}
			andTrue.map{_._1}
		}

		def pnChoices(rowIndex: Int) =
		{
			var pns = SectionTables.tdPNs(rowIndex).pns.keys.filter{rowIndex==0 || _.acceptableConsecutive(pnList(rowIndex-1))}.toList
			val lastRow = displayRows(rowIndex).get
			def isTrue(pn: PN) = getRows(pnList.slice(0,rowIndex)++List(pn)) match
			{
				case Some(rows) => trueAgainstOthers(rows)
				case None => false
			}
			def isValidForHL(pn: PN) =
			{
				val nextRow = lastRow.apply(pn)
				leads.halfleads.contains(nextRow) && pn.acceptableConsecutive(leads.halfleads(nextRow))
			}
			val nextRows = pns.map{(pn)=> lastRow.apply(pn)}
			if (rowIndex==displayLeadLen-2)
				pns = pns.filter(isValidForHL)
			pns = pns.filter(isTrue)
			pns
		}

		private def genAllRows()
		{
			// PN list might have the extra halflead change on it - never generate that
			val rows = getRows(pnList.slice(0,displayLeadLen-1))
			assert(rows.isDefined)
			allRows = rows.get
			assert(trueAgainstOthers(allRows))
		}

		private def getRows(pns: Seq[PN]): Option[MultiBitSet] =
		{
			val buf = ListBuffer[Row]()
			for (lead <- leads.rows)
				buf+= PN.generateChanges(lead, pns, buf)
			val set = MultiBitSet(buf)
			if (set.size==buf.size)
				Some(set)
			else
				None
		}

		private def methodRows(method: NamedMethod): Option[(NamedMethod, MultiBitSet)] =
		{
			getRows(method.lead.slice(0,method.leadLength/2-1)) match
			{
				case Some(set) => Some(Tuple2(method, set))
				case None => None
			}
		}

		private def trueAgainstOthers(mbs: MultiBitSet) =
			methods.forall{(other)=> other.methodNum==methodNum || mbs.trueWith(other.allRows)}
	}

	/** Rubbish brute-force search algorithm... */
	def finishComposition()
	{
		val unfinished = methods.filter{!_.isFinished}.toList
		println("Trying to finish "+unfinished.size+" methods")
		var hiwater = 0

		def search(method: MethodRows, methodsLeft: List[MethodRows]): Boolean =
		{
			val rowIndex = method.nRowsFinished-1
			val success = search2(method, rowIndex, methodsLeft)
			if (!success)
				method.reset(0)
			success
		}

		def search2(method: MethodRows, rowIndex: Int, methodsLeft: List[MethodRows]): Boolean =
		{
			if (rowIndex>=method.displayLeadLen-1)
			{
				val nFound = unfinished.size-methodsLeft.size
				if (nFound>hiwater)
				{
					hiwater = nFound
					println("New hiwater: "+hiwater)
				}
				methodsLeft match
				{
					case m::remaining => search(methodsLeft.head, methodsLeft.tail)
					case Nil => println("FOUND!"); true
				}
			}
			else
			{
				search3(method, rowIndex, method.pnChoices(rowIndex), methodsLeft)
			}
		}

		def search3(method: MethodRows, rowIndex: Int, pnsToTry: List[PN], methodsLeft: List[MethodRows]): Boolean =
		{
			pnsToTry match
			{
				case pn::rest =>
				{
					method.set(rowIndex, pn)
					if (search2(method, rowIndex+1, methodsLeft))
						true
					else
						search3(method, rowIndex, rest, methodsLeft)
				}
				case Nil => false
			}
		}

		search(unfinished.head, unfinished.tail)
		println("Search done")
	}
}