package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition

import scala.collection.immutable.TreeSet
import scala.util.Random

/**
 * @author mark
 */

class SimulatedAnnealingSearch(searchDef: SearchDefinition) extends SearchBase(searchDef)
{
	def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])
	{
		val seedComp = sortedSeeds.head
		out.println("Seeding from: "+seedComp)
		highestMusicComp = seedComp
		highestMusic = seedComp.music
		search(seedComp, 50000000)
	}

	def search(comp: Composition, kMax: Int) =
	{
		//def temperature(k: Int) = 4*(kMax - k).toDouble/kMax
		def temperature(k: Int) = 40*(kMax - k).toDouble/kMax

		var k = 0
		//var state: Composition = new FastComp(comp)
		var state = comp
		var e = energy(state)
		var best = comp
		var bestE = e
		while (k < kMax)
		{
			val t = temperature(k)
			//val newTry = randomMove(state, 0)
			val newTry = randomMoveAllowFalse(state, 0)
			if (newTry.isDefined)
			{
				val newState = newTry.get
				val newE = energy(newState)
				if (acceptMove(e, newE, t))
				{
					state = newState
					e = newE
					if (false && k%200==0)
						out.println("New state at k="+k+": "+state)
					if (newE < bestE)
					{
						//out.println("New best: "+state)
						best = newState.toImmutableComp
						bestE = newE
					}
					keepBest(newState)
				}
				else
				{
					newState.undoLastReplace()
				}
			}
			k+= 1
		}
		best
	}

	def energy(comp: Composition) = -comp.score

	def acceptMove(e: Int, eNew: Int, t: Double) =
	{
		if (eNew<e)
			true
		else
		{
			val p = math.exp( (e-eNew).toDouble/t )
			p > Random.nextDouble()
		}
	}

}