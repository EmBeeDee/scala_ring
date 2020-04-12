package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition

/**
 * @author mark
 */

class ClonedSearchDef(baseDef: SearchDefinition) extends SearchDefinition
{
	val calling = baseDef.calling
	val calls = baseDef.calls
	val musicDefs = baseDef.musicDefs

	val seedProvider = baseDef.seedProvider
	def scoreFn(comp: Composition) = baseDef.scoreFn(comp)

	def getCompPlan = baseDef.getCompPlan

	override def getExcludedLeads = baseDef.getExcludedLeads

	override val logger = baseDef.logger
	override def getCallingMethod = baseDef.getCallingMethod

	override val methods = baseDef.methods

}