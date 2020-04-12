package net.snowtiger.spliced.tables

import net.snowtiger.ringing.{Music, NamedMethod, Row}
import net.snowtiger.spliced.composition.{NodeType, _}


/**
 * A specific instance of a node, from a given starting LH, with a given set of methods.
 * Note that multiparts are handled at the Lead level: a Lead from 2345678 will include the rows and music
 * counts for the 3425678 and 4235678 leadheads for a 3-part on 5678.
 * Care must therefore be taken when calculating lengths or COM (multiply by the number of parts) and
 * especially ATW (when the individual leadheads should be recovered using CompositionPlan.permuteByPartEnds).
 *
 * @param startLH
 * @param nodeType
 * @param splice
 */
case class NodeImpl(startLH: Row, nodeType: NodeType, splice: Splice) extends Node
{
	val length = splice.length
	var music: MusicCount = _

	/** Not called when using lead tables */
	lazy override val allRows: Set[Row] = splice.genAllRows(startLH).toSet

	def methods = splice.methods
	def methodString = splice.methodString
	def methodsUsed = splice.methodsUsed

	override def internalCom = splice.com
	override def countComTo(nextNode: Node) = splice.com + {if (nextNode.isComFrom(splice.lastMethod)) 1 else 0}
	override def isComFrom(method: NamedMethod) = method!=splice.firstMethod

	def genLeads(leadTable: LeadTable) = splice.genLeads(startLH, leadTable)

	def setRank(falseness: Double) { rank = falseness * music(0) * splice.com}

	// Use EITHER the MethodUseTracker, or atw+methodsUsed
	lazy val mut =
	{
		var mut = MethodUseTracker()
		val lhs = splice.genLeadheadsAndMethodsForAllParts(startLH)
		for ((lh,m) <- lhs)
			mut+= MethodUseTracker(m, lh)
		mut
	}

	lazy val atw =
	{
		var atw = MultiAtw()
		val lhs = splice.genLeadheadsAndMethodsForAllParts(startLH)
		for ((lh,m) <- lhs)
			atw+= BitAtw(m, lh)
		atw
	}

	/** If you have created the Node from Leads, populate the music from them instead - this is slower */
	def populateMusic(musicDef: Array[Music]): Unit =
	{
		music = new MusicCount(musicDef).count(allRows)
	}

	/** Note - doesn't intern Leads, so they probably won't be populated with music, false tables etc */
	def toLeads = splice.genLeadheads(startLH).zip(splice.methods).map { (pair) => Lead(pair._1, pair._2) }

	override def toString = "Node "+startLH+" "+nodeType+" "+splice+" "+music

	/** Short form string is a list of method abbreviations with the call at the end. A space is used to show the Home position */
	def shortForm =
	{
		val lhs = splice.genLeadheads(startLH).zip(splice.methods)
		val s = for ((lh,m) <- lhs) yield
		{
			val n = m.nbells
			if (lh.bellAt(n)==n)
				" "+m.toString
			else
				m.toString
		}
		s.mkString + nodeType.endCall /* + (if (leadsWithoutMusic>0) "*" else "") + " " */
	}
}
