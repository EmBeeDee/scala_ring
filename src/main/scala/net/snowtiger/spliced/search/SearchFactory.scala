package net.snowtiger.spliced.search


/**
 * @author mark
 */

trait SearchFactory
{
	def greedy(searchDef: SearchDefinition) =
		new GreedySearch(searchDef).varyFromSeed()

	def onenudge(searchDef: SearchDefinition) =
		new OneNudgeSearch(searchDef).varyFromSeed()

	def squeeze(searchDef: SearchDefinition) =
		new SqueezeSearch(searchDef).varyFromSeed()

	def anneal(searchDef: SearchDefinition) =
		new SimulatedAnnealingSearch(searchDef).varyFromSeed()

	def tunnel(searchDef: SearchDefinition) =
		new StochasticTunnellingSearch(searchDef).varyFromSeed()

	def genetic(searchDef: SearchDefinition) =
		new GeneticSearch(searchDef).varyFromSeed()
}