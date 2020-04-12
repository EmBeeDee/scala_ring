package net.snowtiger.spliced.search

import net.snowtiger.ringing.{Music, NamedMethod}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.tables.{Lead, Node, Splice}

/**
 * @author mark
 */

trait SearchDefinition
{
	val logger = System.out

	val calling: String
	val calls: List[Call]
	val seedProvider: SeedCompProvider

	def scoreFn(comp: Composition):Int
	val musicDefs: Array[Music]

	val methods: List[NamedMethod]
	def getCallingMethod: NamedMethod = methods.last

	def getCompPlan: CompositionPlan

	def getExcludedLeads: Iterable[Lead]

	def acceptSplice(splice: Splice) = true

	def acceptLead(lead: Lead) = true

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	def acceptNode(node: Node, leads: List[Lead]) = true
}