package net.snowtiger.doublehelix

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Method, PN, Perm, Row}

/**
 * @author Mark
 */

object TDFinder
{
	val NBells = 8
	val TreblePath = Array(1,2,1,2, 3,4,3,4, 5,6,5,6, 7,8,7,8)
	val HalfLeadLength = TreblePath.size

	val AllPN = GoodPn(8).noConsec

	val PBPairs = Map(2->Set(Set(3,4), Set(5,6), Set(7,8)),
		3->Set(Set(2,5), Set(4,7), Set(6,8)),
		4->Set(Set(2,6), Set(3,8), Set(5,7)),
		5->Set(Set(3,7), Set(2,8), Set(4,6)),
		6->Set(Set(4,8), Set(2,7), Set(3,5)),
		7->Set(Set(5,8), Set(3,6), Set(2,4)),
		8->Set(Set(6,7), Set(4,5), Set(2,3)))

	var max = 0

	def main(args: Array[String])
	{
		val start = Row("123.....")
		val course1 = (courseFrom(start).toSet-start).map{(r:Row)=> Tuple2(r, true)}
		val course2 = courseFrom(Row("12..3...")).toSet.map{(r:Row)=> Tuple2(r, true)}
		val course3 = courseFrom(Row("12....3.")).toSet.map{(r:Row)=> Tuple2(r, true)}
		val allLeads = course1++course2++course3
		//search((start,true), allLeads, Nil, Set[(Row,Boolean)]())

		//search2(Nil, allLeads.toList.map{ (p)=> Tuple3(List[PN](), p._1, p._2 ) }, allLeads)

		val london = new Method(8, PN.parse("38-38.14-12-38.14-14.58.16-16.58"), PN("12"))
		val masked1stLead = london.fullCourse.slice(0, 32).map(toMaskedPair(_))
		val allLeadsButFirst = (course1-Tuple2(start, true))++course2++course3
		val absolutelyAllLeads = allLeadsButFirst++masked1stLead
		//search2(Nil, allLeadsButFirst.toList.map{ (p)=> Tuple3(List[PN](), p._1, p._2 ) }, absolutelyAllLeads)

		val bristol = new Method(8, PN.parse("-58-14.58-58.36.14-14.58-14-18"), PN("18"))
		val derwent = new Method(8, PN.parse("-56-14-56-36-34-58-34-78"), PN("12"))
		val maskedCourse = derwent.fullCourse.map(toMaskedPair(_))
		search2(Nil, (course2++course3).toList.map{ (p)=> Tuple3(List[PN](), p._1, p._2 ) }, course2++course3++maskedCourse)
	}

	def toMaskedPair(r: Row) = (HelixoidFinder.maskBackFive(r), r.positive)

	def courseFrom(lh: Row) =
	{
		val plainPerm = Perm("13527486")
		def genLeads(nLeft: Int, lhsGenned: List[Row]): List[Row] =
		{
			if (nLeft==0)
				lhsGenned
			else
				genLeads(nLeft-1, lhsGenned.head.apply(plainPerm) :: lhsGenned )
		}
		genLeads(6, List(lh))
	}

	def trebleOnPath(row: Row, n: Int) = row.placeOf(1) == TreblePath(n)

	def pbHalfLeadPlaces(hl: Row) =
	{
		def genPairs(place: Int) =
			(Range(1,place,2).toSet ++ Range(place+1,NBells,2).toSet).map( (i)=>Set(hl.bellAt(i), hl.bellAt(i+1)) )
		Range(1,7,2).map{(i)=>Tuple2(hl.bellAt(i), genPairs(i))}.filter{(p)=> PBPairs(p._1)==p._2 }.map{(p)=>hl.placeOf(p._1)}
	}

	def applyPN(row: (Row,Boolean), pn:PN) = (row._1.apply(pn), pn.swapsSign(8)!=row._2)

	def checkSecondHalfLead(rows: Set[(Row,Boolean)], remainingPN: List[PN], rowsSeen: Set[(Row,Boolean)]): Boolean =
	{
		val n = HalfLeadLength*2-remainingPN.size
		if (n>max) { println("New HL max: "+n); max = n }
		if (remainingPN.isEmpty)
			true
		else
		{
			val pn = remainingPN.head
			val newRows = rows.map{applyPN(_, pn)}
			val newRowsSeen = rowsSeen++rows
			if (newRowsSeen.size==rowsSeen.size+rows.size)
				checkSecondHalfLead(newRows, remainingPN.tail, newRowsSeen)
			else
				false
		}
	}

	def search(primaryRow: (Row, Boolean), otherRows: Set[(Row,Boolean)], revPN: List[PN], rowsSeen: Set[(Row,Boolean)] )
	{
		def output()
		{
			val hlMeth = new Method(8, revPN.reverse)
			val hl = hlMeth.firstLeadEnd
			val pbHls = pbHalfLeadPlaces(hl)
			if (!pbHls.isEmpty)
			{
				val fullHlPn = (PN(""+pbHls.head+"8")::revPN).reverse
				val fullMeth = new Method(8, fullHlPn, PN("12"))
				val rows = otherRows+primaryRow
				println(fullMeth+" "+checkSecondHalfLead(rows, fullHlPn.reverse, rowsSeen))
			}
		}

		val n = 1+revPN.size
		//if (n>max) { println("New max: "+n); max = n }
		val newRowsSeen = (rowsSeen++otherRows)+primaryRow
		if (newRowsSeen.size==rowsSeen.size+otherRows.size+1)
		{
			if (n==HalfLeadLength)
				output()
			else
				for (pn<- AllPN.filter{(pn)=> GoodPn.isAllowableConsecutivePn(pn, revPN)})
				{
					val newRow = applyPN(primaryRow, pn)
					if (trebleOnPath(newRow._1, n) && !rowsSeen.contains(newRow))
						search(newRow, otherRows.map{applyPN(_, pn)}, pn::revPN, newRowsSeen)
				}
		}
	}

	def search2(leadsDone: List[(List[PN], Row, Boolean)], leadsToDo: List[(List[PN], Row, Boolean)], rowsSeen: Set[(Row,Boolean)] )
	{
		def firstPbHalfLead(preHL: Row) =
		{
			val pbHls = pbHalfLeadPlaces(preHL)
			if (pbHls.isEmpty)
				None
			else
				Some(PN(""+pbHls.head+"8"))
		}

		def output(leads: List[(List[PN], Row, Boolean)])
		{
			val preHLs = leads.map( (t)=> new Method(8, t._1.reverse).firstLeadEnd )
			val hlPNs = preHLs.flatMap{ firstPbHalfLead(_) }
			if (hlPNs.size==leads.size)
			{
				val fullHlPn = (hlPNs.head::(leads.head._1)).reverse
				val fullMeth = new Method(8, fullHlPn, PN("12"))
				println(fullMeth)
			}
		}

		leadsToDo match
		{
			case Nil =>
				if (1+leadsDone.head._1.size==HalfLeadLength)
					output(leadsDone)
				else
					search2(Nil, leadsDone, rowsSeen)
			case lead::toDoTail =>
			{
				val n = 1+lead._1.size
				if (n>max) { println("New max: "+n); max = n }
				for (pn<- AllPN.filter{(pn)=> GoodPn.isAllowableConsecutivePn(pn, lead._1)})
				{
					val newRow = applyPN( (lead._2,lead._3), pn)
					if (trebleOnPath(newRow._1, n) && !rowsSeen.contains(newRow))
					{
						val newTriple = (pn::lead._1, newRow._1, newRow._2)
						search2(newTriple::leadsDone, toDoTail, rowsSeen+newRow)
					}

				}
			}
		}
	}

}