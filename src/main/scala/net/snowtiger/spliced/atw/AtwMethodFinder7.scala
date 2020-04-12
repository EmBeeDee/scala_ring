package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.atw.AtwMethodFinder.MethodProvider
import net.snowtiger.spliced.atw.construct.SectionTables.PnTree
import net.snowtiger.spliced.atw.construct.{MethodConstructor, MethodLeads, SectionTables}
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.collection.mutable.ListBuffer

/**
 * This one attempts to complete an existing seed composition which supplies 23 place notations for the
 * first 3 sections of each method. Completion is attempted using the Dancing Links algorithm.
 *
 * @author mark
 */
class AtwMethodFinder7(nbells: Int) extends AtwMethodFinderBase2[MethodNodeWithEndRows](nbells)
{
	/*
	val seed = "Kimys S (b)/3, Muggles S (b)/3, Chadwick S (b)/3, Southwell S (b)/3, Shadwell D (b)/3, Xadier S (j)/3, "+
			"Easthampstead S (l)/3, Crowan S (e)/3, Cricklade S (j)/3, Shadwell D (b)/3, Once Brewed D (g)/3, Praseodymium S (g)/3, "+
			"Sapcote S (f)/3, Lyveden S (g)/3, Bowes-Lyon S (b)/3, Jasonville S (b)/3, Once Brewed D (g)/3, "+
			"Shadwell D (b)/3, Aldgate S (b)/3, Uprington D (g)/3, Kurchatovium S (l)/3, Heliotrope S (h)/3, Redruth S (f)/3"
	*/

	val seed = "Kimys S (b)/3, Fulton S (b)/3, Bowes-Lyon S (b)/3, Ashwell S (b)/3, Lyveden S (g)/3, Stockholm S (d)/3, " +
			"Easthampstead S (l)/3, Vendredi Treize S (k)/3, Edlesborough S (j)/3, Jasonville S (b)/3, Uprington D (g)/3, Shadwell D (b)/3, " +
			"Quy S (f)/3, Bowes-Lyon S (b)/3, Jasonville S (b)/3, Shadwell D (b)/3, Once Brewed D (g)/3, " +
			"Oslo S (b)/3, Bowes-Lyon S (b)/3, Lyveden S (g)/3, Laomedon S (f)/3, Chunky Monkey S (c)/3, Nearest Canal S (l)/3"

	def parseLHG(s: String) = s.substring(s.indexOf('(')+1, s.indexOf(')'))
	def parseMethod(s: String) = s.substring(0, s.indexOf('(')-1).trim

	override protected def makeNode(mc: MethodCourse, sm: SearchMethod) =
	{
		val mn = new MethodNodeWithEndRows(mc, sm)
		if (mn.isTrue) Some(mn) else None
	}

	//methods.groupBy{_.lead.slice(0,sliceSize)}.values.toList.map{FirstSectionSearchMethod(sliceSize,_)}

	def findMethods(methodProvider: MethodProvider, comp: InputComp)
	{
		val methodsByName = methodProvider.allMethods.values.flatten.groupBy(_.namePlusClass)

		println("Building method nodes...")
		val convertBackTo12lhg = MethodConstructor.LHGPairs.map{_.swap}.toMap
		val seedMethods = seed.split(',').map{(s)=> methodsByName(parseMethod(s)).head}
		val seedLHGs = seed.split(',').map{parseLHG}.map{(lhg)=> convertBackTo12lhg.getOrElse(lhg,lhg)}.toList
		//assert(seedMethods.zip(seedLHGs).forall{(p)=> p._1.lhGroup==p._2})

		val lhGroups = comp.unflattenByCourse(seedLHGs)
		val seedSearchMethods = comp.unflattenByCourse(seedMethods.toList.map{(m)=> FirstSectionSearchMethod(11,List(m))}).toArray
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		// Will throw Option.get fail exception if any single method in the seed is false against itself.
		val seedNodes = lhGroups.zipWithIndex.map{(p)=> buildSeedNodes(p._2, cos(p._2), seedSearchMethods(p._2), p._1)}.flatten
		val methodNodes = seedNodes.map{_.oneSectionNodes}

		// Now use the end rows in each seed node to build the actual search nodes - the set of possible finishing sections.

		println("Building matrix...")
		val mandatoryRows = methodNodes.map{(xs)=> xs.head.mandatoryRow}
		val dancingLinks = new DancingLinks
		dancingLinks.createMatrix(mandatoryRows)
		for (nodeList <- methodNodes; node <- nodeList)
			dancingLinks.addMethodRow(node)

		println("Searching...")
		var t = System.currentTimeMillis()
		dancingLinks.search

		t = (System.currentTimeMillis()-t)/1000
		println("Search finished in "+t+"s")
	}

}

class MethodNodeWithEndRows(course: MethodCourse, method: SearchMethod) extends MethodNode(course, method)
{
	var endRows: List[Row] = _
	val possibleHalfLeads = MethodLeads(course, method.lhGroup).halfleads

	/** Extracts the rows nearest the halflead for each lead. */
	override protected def genRows =
	{
		val allRows = super.genRows
		val nHalfleads = course.leadheads.size*2
		val halfleadSize = allRows.size/nHalfleads
		assert(nHalfleads*halfleadSize==allRows.size)
		// This relies on allRows being generated in the right order, with halfleads together each finishing with the end row.
		endRows = allRows.grouped(halfleadSize).map{_.last}.toList
		allRows
	}

	def possibleLastSections: List[List[PN]] =
	{
		val lastRow = endRows.head
		val index = 11
		assert(method.pn.size==index)
		val lastPN = method.pn.last

		var possiblePNs = List[List[PN]]()
		def genPN(pnTree: PnTree, revPn: List[PN], row: Row)
		{
			if (pnTree.isEnd)
				possibleHalfLeads.get(row) match
				{
					case Some(hlpn) => if (hlpn.acceptableConsecutive(revPn.head)) possiblePNs = revPn.reverse.tail::possiblePNs
					case None => // no-op
				}
			else
				for (pn <- pnTree.pns.keys)
					genPN(pnTree.pns(pn), pn::revPn, row.apply(pn))
		}
		genPN(SectionTables.tdPNs(index).acceptableConsecutive(lastPN), List(lastPN), lastRow)
		possiblePNs
	}

	def oneSectionNodes = possibleLastSections.map{(pn)=> new OneSectionMethodNode(this, pn)}.filter(_.isTrue)
}

class OneSectionMethodNode(baseNode: MethodNodeWithEndRows, sectionPn: List[PN]) extends MethodNode(baseNode.course, baseNode.method)
{
	override lazy val isTrue = rows.size==1+course.cos.size*4*2
	def mandatoryRow = baseNode.endRows.head

	/** Calculates the set of all rows for each lead of the method from the given leadheads */
	override protected def genRows =
	{
		val buf = ListBuffer[Row]()
		buf+= mandatoryRow
		for (row <- baseNode.endRows)
		{
			val sectionStart = row.apply(sectionPn.head)
			buf += PN.generateChanges(sectionStart, sectionPn.tail, buf)
		}
		buf.toList
	}

	override def toString = super.toString+"+"+PN.output(sectionPn)
}