package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{Method, NamedMethod, PN, Row}
import net.snowtiger.spliced.composition.Stage

/**
 * @author mark
 */

class MethodConstructor(nbells: Int)
{
	val pnsForFirstSection = List("x","38","36","34","3458","58","56").map(PN(_))
	val pnsForIntersection1 = List("14","1456","1458","16","18").map(PN(_))
	val pnsForSecondSection = List("x","56","58").map(PN(_))
	val pnsForFirstQuarterLead = List(pnsForFirstSection,pnsForFirstSection,pnsForFirstSection,pnsForIntersection1,pnsForSecondSection,pnsForSecondSection,pnsForSecondSection)

	def buildDoubleQuarterLeads() =
	{
		val qlPNs = List("36","18").map(PN(_))
		assert(nbells==8)
		val halfLeadMap = genDoubleHalfleads()
		var byLhGroup = Map[Char,List[List[PN]]]()
		def checkQL(row: Row, revQL: List[PN], qlPN: PN) =
		{
			if (qlPN.acceptableConsecutive(revQL.head))
			{
				val firstQuarter = (qlPN::revQL).reverse
				val lastQuarter = qlPN::(revQL.map{_.reverse(nbells)})
				val halfLead = PN.generateLastRow(row, lastQuarter)
				for (lhGroup <- halfLeadMap.getOrElse(halfLead, Nil))
					byLhGroup+= lhGroup->(firstQuarter::byLhGroup.getOrElse(lhGroup,Nil))
			}
		}
		def build(row: Row, revQL: List[PN], remainingRows: List[List[PN]])
		{
			remainingRows match
			{
				case Nil => qlPNs.foreach{checkQL(row, revQL, _)}
				case pnChoices::rest =>
					for (pn<-pnChoices; if (revQL.isEmpty || revQL.head.acceptableConsecutive(pn)))
						build(row.apply(pn), pn::revQL, rest)
			}
		}
		build(Row(nbells), Nil, pnsForFirstQuarterLead)
		byLhGroup
	}

	def genDoubleHalfleads() =
	{
		var halfLeadMap = Map[Row,List[Char]]()
		var lhGroup = 'a'
		for (leadend <- allPBLeadends)
		{
			def isDouble(halfLead: Row) =
			{
				//halfLead.isPlainBob
				///*
				val row = halfLead.reverse
				val fixedBells = row.bellsInSamePlace(leadend.rounds).toList
				var pairs = Method.pairSwaps(row)
				fixedBells.size==2 && !fixedBells.contains(2) && 2*pairs.size==row.nbells-2
				//*/
			}
			val doubleHalfleads = Method.genAllHalfLeadsForLeadEnd(leadend).filter{isDouble}
			for (halfLead <- doubleHalfleads)
				halfLeadMap+= halfLead->(lhGroup::halfLeadMap.getOrElse(halfLead,Nil))
			lhGroup = (lhGroup+1).toChar
		}
		halfLeadMap
	}

	def doubleMethods() =
	{
		var allMethods = List[NamedMethod]()
		val fixedHLBells = Map('a'->8, 'b'->3, 'c'->6, 'd'->5, 'e'->4, 'f'->7)
		val rounds = Row(nbells)
		val doubleQL = buildDoubleQuarterLeads()
		for (lhGroup <- doubleQL.keys.toList.sorted)
		{
			val fixedBell = fixedHLBells(lhGroup)
			for (qlPN <- doubleQL(lhGroup))
			{
				val pn = qlPN++(qlPN.reverse.tail.map{_.reverse(nbells)})
				val hl = PN.generateLastRow(rounds, pn)
				val hlPN = PN(""+hl.placeOf(fixedBell)+"8")
				val pnString = PN.output(pn++List(hlPN))
				val method = NamedMethod(pnString, nbells, pnString, "12")
				allMethods = method::allMethods
			}
		}
		allMethods
	}

	val pnsForLastSection = List("x","12","14","16","34","36","56").map(PN(_))

	def buildLastSectionsFrom(hl1: Row, hl2: Row) = buildSectionsFrom(hl1, hl2, pnsForLastSection)

	def buildSectionsFrom(hl1: Row, hl2: Row, pns: List[PN]) =
	{
		var sections: List[List[Row]] = Nil
		val row4 = hl1
		val row5 = hl2
		for (pn1 <- pns)
		{
			val row3 = row4.apply(pn1)
			val row6 = row5.apply(pn1)
			for (pn2 <- pns; if !pn1.hasConsecutivePlacesWith(pn2))
			{
				val row2 = row3.apply(pn2)
				val row7 = row6.apply(pn2)
				if (Set(row2,row4,row5,row7).size==4)
					for (pn3 <- pns; if pn2.acceptableConsecutive(pn3))
					{
						val row1 = row2.apply(pn3)
						val row8 = row7.apply(pn3)
						if (Set(row1,row3,row6,row8).size==4)
						  sections = List(row1,row2,row3,row4,row5,row6,row7,row8)::sections
					}
			}
		}
		sections
	}

	def allPBLeadends =
	{
		val pn12 = PN("12")
		val plainCourseLeadends = new Stage(nbells).StageLeadheads.filter{!_.isRounds}.map{_.apply(pn12)}
		plainCourseLeadends
	}


}

object MethodConstructor
{
	def main(args: Array[String])
	{
		val constructor = new MethodConstructor(8)
		val sections = constructor.buildLastSectionsFrom(Row("23456781"), Row("32547681"))
		for (section <- sections)
			println(section)
		println(sections.size)
		println
		for (leadend <- constructor.allPBLeadends)
			println(leadend+" "+Method.genHalfLeadPairsForLeadEnd(leadend).size)
		println

		val doubleQL = constructor.buildDoubleQuarterLeads()
		for (lhGroup <- doubleQL.keys.toList.sorted)
			println(lhGroup+" "+doubleQL(lhGroup).size+" "+doubleQL(lhGroup).map{PN.output(_)}.mkString(", "))
	}

	def doubleMethods() = new MethodConstructor(8).doubleMethods()

	/** Pairs of 12 and 18 lhgroups which have the same halflead (NOT leadhead!) */
	val LHGPairs = List(("a","a"), ("b","g"), ("c","h"), ("d","j"), ("e","k"), ("f","l"), ("m","m") )

}