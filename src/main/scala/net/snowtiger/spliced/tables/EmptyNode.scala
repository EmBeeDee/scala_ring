package net.snowtiger.spliced.tables

import net.snowtiger.ringing.{NamedMethod, Perm, Row}
import net.snowtiger.spliced.composition._

import scala.collection.immutable.BitSet

/**
 * @author mark
 */

case class EmptyNode(cloneNode: Node) extends Node
{
	falseLeads = BitSet()
	leads = BitSet()

	val startLH = cloneNode.startLH
	val nodeType = cloneNode.nodeType
	val length = 0
	var music: MusicCount = new MusicCount(cloneNode.music.defs)

	override val allRows: Set[Row] = Set()

	def methods = Nil
	def methodString = ""
	def methodsUsed = Set()

	override def internalCom = 0
	override def countComTo(nextNode: Node) = 0
	override def isComFrom(method: NamedMethod) = false
	def genRows() = Nil
	def genLeads(leadTable: LeadTable) = Nil

	def setRank(falseness: Double) {rank = falseness}

	override def mut = MethodUseTracker()
	override def atw = MultiAtw()

	/** Note - doesn't intern Leads, so they probably won't be populated with music, false tables etc */
	def toLeads = Nil

	override def toString = "Node "+startLH+" "+nodeType+" EMPTY "+music

	/** Short form string is a list of method abbreviations with the call at the end. A space is used to show the Home position */
	def shortForm = startLH.rotateUntilBellHome(8, new Perm("13527486"))+"c "
}