package net.snowtiger.ringing

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashSet, ListBuffer}

/**
 * @author Mark
 */

class Method(val nbells: Int, val lead: Seq[PN])
{
	val leadLength = lead.size
	lazy val leadheadPN = lead.last
	def firstHalfPN = lead.slice(0,leadLength/2)
	def halfleadPN = lead(leadLength/2-1)
	lazy val leadHeads = generateLeadHeads()
	lazy val nLeads = leadHeads.size
	lazy val courseLength = leadLength * nLeads
	private var plainCourse = ArrayBuffer(Row(nbells))
	lazy val firstLead = generatePlainLeads(1)
	lazy val firstLeadEnd = {firstLead; plainCourse(leadLength-1)}
	lazy val firstLeadHead = {firstLead; plainCourse(leadLength)}
	lazy val fullCourse: List[Row] = generatePlainLeads(nLeads)

	def this(m: Method) = this(m.nbells, m.lead)
	def this(nbells: Int, pn: String) = this(nbells, PN.parse(pn, nbells))

	/** Constructors for symmetrical methods */
	def this(nbells: Int, halfLeadPn: Seq[PN], leadHead: PN) = this(nbells, halfLeadPn++((leadHead+:halfLeadPn).reverse.tail))
	def this(nbells: Int, halfLeadPn: String, leadHead: String) = this(nbells, PN.parse(halfLeadPn, nbells), PN(leadHead))

	override def hashCode() = lead.hashCode()

	/** Equality based on equivalence of place notation - but does not check rotations */
	override def equals(obj: scala.Any) = obj match
	{
		case other: Method => other.lead==lead
		case _ => false
	}

	/** A permutation taking one leadhead to the next by a plain lead */
	def plainPerm = leadHeads.head.toPerm

	/** A permutation taking one leadhead to the next by a call (assumed to be a single PN affecting the leadhead) */
	def callPerm(call: PN) = firstLeadEnd.apply(call).toPerm

	private def generateLeadHeads(): List[Row] =
	{
		// TODO could replace with call to perm.generateGroupFrom(Row(nbells)) but would need to strip off rounds and add to end
		val perm = firstLeadHead.toPerm
		var lh = firstLeadHead
		val results = ListBuffer(lh)
		while (!lh.isRounds)
		{
			lh = lh.apply(perm)
			results+= lh
		}
		results.toList
	}

	private def generatePlainLeads(leads: Int): List[Row] =
	{
		val changesNeeded = leads*leadLength
		if (plainCourse.length<changesNeeded)
		{
			var r = plainCourse.last
			r = PN.generateChangesExcludingStart(r, lead, plainCourse)
			while (!r.isRounds && plainCourse.length<changesNeeded)
				r = PN.generateChangesExcludingStart(r, lead, plainCourse)
		}
		plainCourse.take(changesNeeded).toList
	}

	/**
	 * Fills the buffer with the rows in one plain lead of the method, starting with the start row, but excluding
	 * the finish row, which is instead returned.
	 * @param start
	 * @param builder
	 * @return
	 */
	def generateLead(start: Row, builder: mutable.Buffer[Row]): Row = PN.generateChanges(start, lead, builder)

	def generateLead(start: Row): List[Row] =
	{
		val ret = new ListBuffer[Row]()
		generateLead(start, ret)
		ret.toList
	}

	def generateFullCourse(startRow: Row): List[Row] =
	{
		val ret = new ListBuffer[Row]
		var r = startRow
		r = generateLead(r, ret)
		while (r!=startRow)
			r = generateLead(r, ret)
		ret.toList
	}

	def isTrue =
	{
		val set: HashSet[Row] = HashSet.empty
		def checkTrue(xs: List[Row]): Boolean =
		{
			xs match
			{
				case Nil => true
				case r :: tail => set.add(r) && checkTrue(tail)
			}
		}
		checkTrue(fullCourse)
	}

	/** Returns the count of changes with wrong-place PN divided by the course length.
		* TODO: currently seems to allow Kent places! */
	def wrongPlaceFactor: Double = wrongPlaceFactor(lead)

	def wrongPlaceFactor(pns: Seq[PN]): Double =
	{
		var count = 0
		var even = true
		for (pn<-pns)
		{
			if (even)
				count+= pn.oddPairsCrossing(nbells)
			/* Old system of counting whether odd or even rows are the x notation does work well if e.g. 56 pns are used
			if ((pn.isCross && !even) || (!pn.isCross && even))
				count+= 1
			*/
			even = !even
		}
		count.toDouble/lead.size
	}

	/** Returns true if bells 7 and up fall into a position found in one of the plain course leadheads of the method */
	def isTenorsTogetherLeadHead(lh: Row) = leadHeads.map(_.maskFrontBells(6)).contains(lh)

	lazy val isSymmetric =
	{
		val halfLead = lead.length/2
		2*(halfLead)==lead.length && lead.slice(0,halfLead-1)==lead.slice(halfLead,lead.length-1).reverse
	}

	/** List of positions the given bell takes through the entire plain course */
	def path(fromBell: Int) =	fullCourse.map{_.placeOf(fromBell)}

	override def toString() =
	{
		"Method: " + outputPN() + " = " + firstLeadHead
	}

	def outputPN() =
	{
		if (isSymmetric)
			PN.output(firstHalfPN)+", "+leadheadPN
		else
			PN.output(lead)
	}
}

object Method
{
	def genHalfLeadPairsForLeadEnd(leadend: Row): Set[(Row,Row)] =
	{
		val fixedBells = leadend.bellsInSamePlace(leadend.rounds).toList
		def halfLeadPN(halflead: Row) = PN(fixedBells.map{halflead.placeOf(_)-1}.sorted)
		val halfLeads = genAllHalfLeadsForLeadEnd(leadend).toSet
		val sets = halfLeads.map{h=> Set(h, h.apply(halfLeadPN(h)))}
		sets.map{(p)=> (p.head, p.tail.head)}
	}

	def genAllHalfLeadsForLeadEnd(leadend: Row): List[Row] =
	{
		val fixedBells = leadend.bellsInSamePlace(leadend.rounds).toList
		var pairs = pairSwaps(leadend)
		if (fixedBells.size+2*pairs.size < leadend.nbells)
			Nil
		else if (fixedBells.head==1)
			genAllHalfLeadsFor(fixedBells.tail, pairs).map{(r)=> Row((1::r).reverse)}
		else
			genAllHalfLeadsFor(fixedBells, pairs).map{Row(_)}
	}

	def pairSwaps(row: Row): List[(Int,Int)] =
	{
		var pairs: List[(Int,Int)] = Nil
		for (i <- 1 to row.nbells)
		{
			val b = row.bellAt(i)
			if (b>i)
			{
				val b2 = row.bellAt(b)
				if (b2==i)
					pairs = (b,b2)::pairs
			}
		}
		pairs
	}

	private def genAllHalfLeadsFor(fixedBells: List[Int], pairs: List[(Int,Int)]): List[List[Int]] =
	{
		val halfLeads = ListBuffer[List[Int]]()

		def generate(halfLead: List[Int], fixedBellsLeft: Array[Int], pairsLeft: Array[(Int,Int)])
		{
			if (fixedBellsLeft.size==0 && pairsLeft.size==0)
		    halfLeads+= halfLead
			else
			{
				for (i <- 0 until fixedBellsLeft.size)
				{
					val newFixedBells = fixedBellsLeft.take(i)++fixedBellsLeft.drop(i+1)
					generate(fixedBellsLeft(i)::halfLead, newFixedBells, pairsLeft)
				}
				for (i <- 0 until pairsLeft.size)
				{
					val newPairs = pairsLeft.take(i)++pairsLeft.drop(i+1)
					generate(pairsLeft(i)._1::pairsLeft(i)._2::halfLead, fixedBellsLeft, newPairs)
					generate(pairsLeft(i)._2::pairsLeft(i)._1::halfLead, fixedBellsLeft, newPairs)
				}
			}
		}

		generate(Nil, fixedBells.toArray, pairs.toArray)
		halfLeads.toList
	}

	def main(args: Array[String])
	{
		test()
	}

	private def test()
	{
		println(genAllHalfLeadsForLeadEnd(Row("13254768")))
	}

}