package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.atw.{MethodCourse, MultiBitSet, PartialMethodNodeBitSet, RowBitSet}

/**
 * @author mark
 */
class LastSectionBuilder(methodCourses: List[MethodCourse], possibleHalfLeads: List[Map[Row,PN]]) extends TreblePath
{
	val sections: Array[Map[Row,LastSections]] = methodCourses.zip(possibleHalfLeads).map{(p)=> buildSections(p._1,p._2)}.toArray
	var hiwater = 0

	def completeComp(partialComp: List[PartialMethodNodeBitSet]): Unit =
	{
		val methods = partialComp.map{MethodToBeFinished(_)}
		hiwater = 0
		complete(RowBitSet(7), RowBitSet(8), methods, Nil )
	}

	private def complete(bitset7: RowBitSet, bitset8: RowBitSet, toFinish: List[MethodToBeFinished], revFinished: List[MethodFinished]): Unit =
	{
		if (toFinish.isEmpty)
		{
			println("SOLUTION: " + revFinished.reverse.mkString(", "))
		}
		else
		{
			val n = revFinished.size
			if (n>hiwater)
			{
				println("HIWATER: " + n)
				hiwater = n
			}
			val tbc = toFinish.head
			val sectionOptions = sections(n)
			for (row <- tbc.startRows.keys)
				sectionOptions.get(row) match
				{
					case Some(lastSection) =>
					{
						val (pn,rbs) = tbc.startRows(row)
						for ( ((rbs7,rbs8), pnSet)  <- lastSection.availableFrom(pn, bitset7, bitset8))
						{
							val newMethod = MethodFinished(tbc, pn, pnSet.head)
							complete(bitset7+rbs7, bitset8+rbs8, toFinish.tail, newMethod::revFinished)
						}
					}
					case None => // no-op
				}
		}
	}

	def buildSections(methodCourse: MethodCourse, halfLeads: Map[Row,PN]): Map[Row,LastSections] =
	{
		println("Build last section "+methodCourse)
		val firstLH = methodCourse.leadheads.head
		def permHLwithinCourse(lh: Row, row: Row) = row.permuteBy(firstLH.permutationBetween(lh))
		var result = Map[Row,LastSections]()
		for (hl <- halfLeads.keys)
		{
			val pn = halfLeads(hl)
			val handstrokeHLs = methodCourse.leadheads.map{permHLwithinCourse(_,hl)}
			val backstrokeHLs = handstrokeHLs.map(_.apply(pn))
			result = buildSections(hl, handstrokeHLs++backstrokeHLs, pn, result)
		}
		result
	}

	private def buildSections(hl: Row, allHLrows: List[Row], hlPN: PN, lastSections: Map[Row,LastSections]): Map[Row,LastSections] =
	{
		var result = lastSections
		val secPNs = SectionTables.sec4
		val bitset0 = RowBitSet(allHLrows)
		for (pn1 <- secPNs; if pn1.acceptableConsecutive(hlPN))
		{
			var rows1 = allHLrows.map(_.apply(pn1))
			val bitset1 = RowBitSet(rows1)
			if (SectionSizes(3)==2)
			{
				val startRow = hl.apply(pn1)
				val newLastSections = result.getOrElse(startRow, new LastSections()) .add(bitset1, bitset0, List(pn1))
				result = result + (startRow->newLastSections)
			}
			else
			{
				for (pn2 <- secPNs; if pn2.acceptableConsecutive(pn1))
				{
					var rows2 = rows1.map(_.apply(pn2))
					val bitset2 = RowBitSet(rows2)+bitset0
					if (bitset2.size==7*2)
						for (pn3 <- secPNs; if pn3.acceptableConsecutive(pn2))
						{
							var rows3 = rows2.map(_.apply(pn3))
							val bitset3 = RowBitSet(rows3)+bitset1
							if (bitset3.size==7*2)
							{
								val startRow = hl.apply(pn1).apply(pn2).apply(pn3)
								val newLastSections = result.getOrElse(startRow, new LastSections()) .add(bitset3, bitset2, List(pn3,pn2,pn1))
								result = result + (startRow->newLastSections)
							}
						}
				}
			}
		}
		result
	}
}

case class LastSections(possibles: Map[(RowBitSet,RowBitSet),Set[List[PN]]])
{
	def this() = this(Map())

	def add(bitset7: RowBitSet, bitset8: RowBitSet, pn: List[PN]) =
	{
		val bitsets = (bitset7,bitset8)
		val newPossible = possibles.getOrElse(bitsets,Set()) + pn
		LastSections(possibles + (bitsets->newPossible))
	}

	def availableFrom(pn: PN, all7: RowBitSet, all8: RowBitSet) =
	{
		possibles.filter{ p => p._2.exists(_.head.acceptableConsecutive(pn)) && all7.trueWith(p._1._1) && all8.trueWith(p._1._2) }
	}

	def availableFrom(pn: PN, allRows: MultiBitSet) =
	{
		possibles.filter{ p => p._2.exists(_.head.acceptableConsecutive(pn)) && allRows.trueWith(p._1._1) && allRows.trueWith(p._1._2) }
	}
}

case class MethodToBeFinished(partial: PartialMethodNodeBitSet)
{
	assert(partial.effectiveSections==3)
	// End of 3rd section 
	val lastPN = partial.lastPN.head
	val endRows = partial.node.sectionEnds(3)
	val endRow = endRows.head
	// Possibilities for start of 4th section: Map row -> (pn, bitset8)
	val startRows = SectionTables.int3.filter{_.acceptableConsecutive(lastPN)}.map{(pn)=> (endRow.apply(pn), (pn, RowBitSet(endRows.map{_.apply(pn)})) )}.toMap
}

case class MethodFinished(tbc: MethodToBeFinished, intPN: PN, lastSection: List[PN])
{
	def displayPN = (intPN::tbc.partial.lastPN).reverse ++ lastSection
	override def toString = PN.output(displayPN) + " ("+tbc.partial.node.method.lhGroup+")"
}