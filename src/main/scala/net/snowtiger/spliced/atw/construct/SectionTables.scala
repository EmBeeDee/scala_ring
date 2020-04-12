package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{PN, Row}

import scala.collection.mutable

/**
 * @author mark
 */

object SectionTables extends TreblePath
{
	val nbells = 8	// Bitsets only work for 8 bells

	def acceptPN(pn: PN) = true //pn.nPlaces<=2
	def simplePN(pnList: List[List[PN]]) = pnList.map{_.filter(_.nPlaces<=2)}
	def vsimplePN(pnList: List[List[PN]]) = pnList.map{_.filter(_.nPlaces<=2).filter(!_.isPlace(2))}
	def surpriseOnly(pnList: List[List[PN]]) = pnList.map{_.filter(_!=PN("18"))}
	def isDelight(pn: List[PN]) = pn.exists{_==PN("18")}

	val sec1 = List("x","38","36","34","3458","58","56").map(PN(_)).filter{acceptPN}
	val int1 = List("14","1456","1458","16","18").map(PN(_)).filter{acceptPN}
	val simpleInt1 = List("14","1458","16","18").map(PN(_)).filter{acceptPN}
	val sec2 = List("x","12","1256","1258","56","58").map(PN(_)).filter{acceptPN}
	val int2 = List("16","18","1236","1238","36","38").map(PN(_)).filter{acceptPN}
	val sec3 = List("x","12","14","34","1234").map(PN(_)).filter{acceptPN}
	val int3 = List("18","1238","1258","1458","38","3458","58").map(PN(_)).filter{acceptPN}
	val simpleInt3 = List("18","1258","1458","38","58").map(PN(_)).filter{acceptPN}
	val sec4 = List("x","12","14","16","1234","1236","1456","34","36","3456","56").map(PN(_)).filter{acceptPN}

	val sec1List = List.fill(SectionSizes(0)-1){sec1}
	val sec2List = List.fill(SectionSizes(1)-1){sec2}
	val sec3List = List.fill(SectionSizes(2)-1){sec3}
	val sec4List = List.fill(SectionSizes(3)-1){sec4}

	val endTree = PnTree(Map())
	val section4 = buildSectionMiddle(sec4, SectionSizes(3)-1, endTree)
	val section3end = buildSectionEnd(int3, sec4, SectionSizes(3), endTree)
	val section3 = buildSection(sec3, int3, SectionSizes(2), section3end(0), section4(0))
	val section2end = buildSectionEnd(int2, sec3, SectionSizes(2), section3end(0))
	val section2 = buildSection(sec2, int2, SectionSizes(1), section2end(0), section3(0))
	val section1end = buildSectionEnd(int1, sec2, SectionSizes(1), section2end(0))
	val section1 = buildSection(sec1, int1, SectionSizes(0), section1end(0), section2(0))

	val tdPNs = section1++section1end++section2++section2end++section3++section3end++section4++Array(endTree)

	val stalactitePNs = simplePN(sec1List)++List(simpleInt1)++simplePN(sec2List++List(int2)++sec3List)++List(simpleInt3)
	val stalagLinks = simpleInt3
	val stalagmitePNs = simplePN(sec4List)

	def buildSection(sec: List[PN], int: List[PN], nRows: Int, thisSectionEnd: PnTree, nextSectionStart: PnTree): Array[PnTree] =
	{
		buildSectionStart(sec, int, nRows, nextSectionStart) ++ buildSectionMiddle(sec, nRows-2, thisSectionEnd)
	}

	def buildSectionStart(sec: List[PN], int: List[PN], nRows: Int, nextSectionStart: PnTree): Array[PnTree] =
	{
		Array( buildPnTree(List.fill(nRows-1)(sec)++List(int), nextSectionStart, true) )
	}

	def buildSectionMiddle(sec: List[PN], nRows: Int, thisSectionEnd: PnTree): Array[PnTree] =
	{
		var result = List[PnTree]()
		var nextSection = thisSectionEnd
		var i = nRows
		while (i>0)
		{
			nextSection = buildPnTree(sec, nextSection)
			result = nextSection::result
			i-= 1
		}
		result.toArray
	}

	def buildSectionEnd(int: List[PN], sec: List[PN], nRows: Int, nextSectionEnd: PnTree): Array[PnTree] =
	{
		Array( buildPnTree(int::List.fill(nRows-1)(sec), nextSectionEnd, true) )
	}

	def buildPnTree(rowPns: List[PN], nextTree: PnTree): PnTree = buildPnTree(List(rowPns), nextTree, false)

	def buildPnTree(pnsPerRow: List[List[PN]], nextTree: PnTree, wholeSection: Boolean): PnTree =
	{
		val rounds = Row(nbells)
		val previousSections = mutable.Set[(Row,Set[Row])]()

		def buildMap(pn: PN, remaining: List[List[PN]], rows: Set[Row]): Option[(PN,PnTree)] =
		{
			val newRow = rows.head.apply(pn)
			if (previousSections.contains( (newRow, rows) ))
				None
			else
				Some( (pn, buildTree(pn, remaining, newRow, rows)) )
		}

		def buildTree(lastPN: PN, remaining: List[List[PN]], newRow: Row, rows: Set[Row]): PnTree = remaining match
		{
			case pns::rest =>
			{
				val nextRows = rows+newRow
				PnTree(Map[PN,PnTree]()++pns.filter{_.acceptableConsecutive(lastPN)}.flatMap{buildMap(_, rest, nextRows)})
			}
			case Nil =>
			{
				if (wholeSection)
					previousSections+= Tuple2(newRow, rows)
				nextTree.acceptableConsecutive(lastPN)
			}
		}

		PnTree(Map[PN,PnTree]()++pnsPerRow.head.flatMap{buildMap(_, pnsPerRow.tail, Set(rounds))})
	}

	case class PnTree(pns: Map[PN,PnTree])
	{
		def isEnd = pns.isEmpty
		def acceptableConsecutive(pn: PN): PnTree = PnTree(pns.filterKeys{_.acceptableConsecutive(pn)}.view.force)
	}
}