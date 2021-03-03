package net.snowtiger.spliced.tables

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition.{NodeType, _}

import scala.collection.BitSet

/**
 * A specific instance of a node, from a given starting LH, with a given set of methods.
 * Note that multiparts are handled at the Lead level: a Lead from 2345678 will include the rows and music
 * counts for the 3425678 and 4235678 leadheads for a 3-part on 5678.
 * Care must therefore be taken when calculating lengths or COM (multiply by the number of parts) and
 * especially ATW (when the individual leadheads should be recovered using CompositionPlan.permuteByPartEnds).
 */
trait Node extends Numberable
{
	val startLH: Row
	val nodeType: NodeType
	val length: Int
	var music: MusicCount
	var leadsWithoutMusic = 0
	var falseLeads: BitSet = _
	var internallyFalse = 0
	var leads: BitSet = _
	var rank: Double = 0.0

	var isInTable = false

	/** Don't call this if using lead tables - slow and memory-expensive! */
	val allRows: Set[Row]

	def methods: List[NamedMethod]
	def size = methods.size
	def methodString: String

	// Use EITHER the MethodUseTracker, or atw+methodsUsed
	def mut: MethodUseTracker
	def atw: MultiAtw
	def methodsUsed: Set[NamedMethod]

	def internalCom: Int
	def countComTo(nextNode: Node): Int
	def isComFrom(method: NamedMethod): Boolean
	def genLeads(leadTable: LeadTable): List[Lead]

	def setRank(falseness: Double)

	/** Note - doesn't intern Leads, so they probably won't be populated with music, false tables etc */
	def toLeads: List[Lead]

	/** Short form string is a list of method abbreviations with the call at the end. A space is used to show the Home position */
	def shortForm: String

	def notFalseAgainst(falseLeadBits: BitSet): Boolean = (leads&falseLeadBits).isEmpty
	def trueWith(leadBits: BitSet): Boolean = (falseLeads&leadBits).isEmpty
	def trueWith(nodes: Iterable[Node]): Boolean = trueWith(Node.leadBitSet(nodes))


}

object Node
{
	def apply(startLH: Row, nodeType: NodeType, splice: Splice) = new NodeImpl(startLH, nodeType, splice)
	/*
	{
		override def shortForm = ""
	}
	*/

	def leadBitSet(nodes: Iterable[Node]): BitSet = leadBitSet(nodes, BitSet())
	def leadBitSet(nodes: Iterable[Node], bitset: BitSet): BitSet =
	{
		if (nodes.isEmpty)
			bitset
		else
			nodes.foldLeft(bitset){ (bits, node)=> bits|node.leads}
	}

	def falseLeadBitSet(nodes: Iterable[Node]): BitSet = falseLeadBitSet(nodes, BitSet())
	def falseLeadBitSet(nodes: Iterable[Node], bitset: BitSet): BitSet =
	{
		if (nodes.isEmpty)
			bitset
		else
			nodes.foldLeft(bitset){ (bits, node)=> bits|node.falseLeads}
	}
}

class NodeTable extends Numberer[Node]