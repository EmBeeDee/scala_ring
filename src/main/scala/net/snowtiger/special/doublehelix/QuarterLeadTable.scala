package net.snowtiger.doublehelix

import net.snowtiger.ringing.{PN, Row}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Table of pre-quarter-leads which are able to produce a valid (3,5)-differential leadhead, for a Double method.
 * Valid pre-quarter-leads are mapped to the set of PNs which produce valid post-quarter-lead rows.
 * Use is made of a HalfLeadTable to identify valid halfleads.
 *
 * @author Mark
 */

class QuarterLeadTable(halfLeadTable: HalfLeadTable)
{
	val AllowedPN = List("-", "18", "36").map(PN(_))

	val quarterLeadTable = buildQuarterLeadTable()

	def isValid(preQuarterLead: Row) = quarterLeadTable.contains(preQuarterLead)

	/** Returns List of pairs (quarterLeadPN, preHalfLeadRow) */
	def getValidQuarterLeads(preQuarterLead: Row) = quarterLeadTable(preQuarterLead)

	/**
	 * Table build
	 * @return
	 */
	private def buildQuarterLeadTable(): Map[Row, List[(PN, Row)]] =
	{
		println("Generating quarter-lead table...")
		val rounds = Row(8)
		val quarterLeadMap = mutable.Map[Row, List[(PN, Row)]]()
		for (row <- Row.generateAll(8))
		{
			val workingPNs = ListBuffer[(PN, Row)]()
			for (pn <- AllowedPN)
			{
				val reverseQuarterLead = row.apply(pn).reverse
				val revPreHalfLead = rounds.reflectAroundPivot(row, reverseQuarterLead)
				if (revPreHalfLead.isDefined)
				{
					val preHalfLead = revPreHalfLead.get.reverse
					if (halfLeadTable.getHalfleadPnMap(preHalfLead).isDefined)
						workingPNs+= Tuple2(pn, preHalfLead)
				}
			}
			if (!workingPNs.isEmpty)
				quarterLeadMap(row) = workingPNs.toList
		}
		println("Done: "+quarterLeadMap.size+" quarter-leads")
		quarterLeadMap.toMap
	}
}