package net.snowtiger.spliced.tables

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition.{OnePart, PartEndPermutes}

import scala.collection.mutable.ListBuffer

/**
 * A Splice is a sequence of plain leads, each of which may be a different method.
 * @param methods
 */
case class Splice(methods: List[NamedMethod], partEnds: PartEndPermutes) extends Ordered[Splice]
{
	def this(methods: List[NamedMethod]) = this(methods, OnePart)

	val firstMethod = methods.head
	val lastMethod = methods.reverse.head
	val methodString = methods.mkString
	val com = methods.foldLeft(Tuple2(firstMethod, 0)){ (t, m) => if (t._1==m) (m, t._2) else (m, t._2+1) }._2
	val comPerLead = com.toDouble/methods.size
	def nLeads = methods.length
	val length = methods.map{_.leadLength}.sum
	val methodsUsed = methods.toSet

	def isInSingleMethodComp(m: NamedMethod) = com==0 && m==firstMethod

	/** Nodes with higher comPerLead values are favoured, but must make sure the comparator gives different
		* results for two different Nodes even if com are identical, in order to work properly in Sets.
		* @param that
		* @return
		*/
	def compare(that: Splice) =
	{
		val c = comPerLead.compare(that.comPerLead)
		if (c==0)
			methodString.compare(that.methodString)
		else
			c
	}

	/** Note for multiparts, each Lead contains all the changes for the leads in the same position in each part */
	def genLeads(from: Row, leadTable: LeadTable): List[Lead] =
	{
		var lh = from
		def nextLead(m: NamedMethod) =
		{
			val curr = lh;
			lh = curr.apply(m.plainPerm);
			leadTable.getInterned(Lead(curr, m))
		}
		methods.map {nextLead(_)}
	}

	/** Just generates the leadheads for the first part, if a multipart */
	def genLeadheads(from: Row) =
	{
		var lh = from
		for (m <- methods)
			yield {val curr = lh; lh = curr.apply(m.plainPerm); curr}
	}

	def genLeadheadsForAllParts(from: Row): List[Row] =
	{
		genLeadheads(from).flatMap( partEnds.permuteByPartEnds(_) )
	}

	/** TODO make lazy val? */
	def genLeadheadsAndMethodsForAllParts(from: Row): List[(Row,NamedMethod)] =
	{
		genLeadheadsForAllParts(from).zip( methods.flatMap{ (m)=> List.fill(partEnds.nparts)(m) } )
	}

	def genAllRows(from: Row): List[Row] =
	{
		val rows = ListBuffer[Row]()
		for ((r,m) <- genLeadheadsAndMethodsForAllParts(from))
			m.generateLead(r, rows)
		rows.toList
	}

	override def toString = methods.mkString
}

