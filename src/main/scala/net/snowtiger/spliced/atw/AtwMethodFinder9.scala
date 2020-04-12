package net.snowtiger.spliced.atw

import net.snowtiger.spliced.atw.construct.{CompositionState, FinishedComposition, SearchPosition, SolutionOutputter}

import scala.util.Random

/**
 * The simulated annealing algorithm applied to the results of a slalactite/stalagmite table build, i.e. running
 * across all possible methods for the composition. Uses fixed splices in natural splice order.
 *
 * @author mark
 */
class AtwMethodFinder9(nbells: Int) extends AtwMethodFinder8(nbells)
{
	override def search(searchPositions: Vector[SearchPosition]) =
	{
		val comp = new Composition(searchPositions)
		comp.seedFrom("*** SOLUTION: 840 (147/35,13462578): C0.0/-58-1458-56-16-14-58.36-56.38 (21) C0.4/-56-14-56-38.12-12.58.14-14.58,56-56.14.56-56.38.12-12.58.14-14.58 (60) C0.3/-58-14.56-56.38-12-58.34-16.78 (32) C0.6/-36-16-56-38.12-12.58.16-12.78 (17) C0.1/38-56.14-12-38.12-12.38-34-58 (43) C0.2/56-56.14-12-38-12-38.14-36.58 (52) C1.0/-36-14.56-56.38-14-58.34-36.18 (25) C1.4/-58-1458-12-38-12-58-12-38,-58-1458-12-38.12-12.58-12-38,-58-1458-12-38-12-58.12-12.38,-58-1458-12-38.12-12.58.12-12.38,-58-1458-12-38-12-1258-12-38 (37) C1.3/34-56.14.56-56.38-12-58-12-18 (63) C1.5/58-56.14-56-36-12-58.12-14.78 (49) C1.6/-38-14.56-56.38-14-38.12-12.78,-38-14.56-56.38-14-38-12-78 (55) C1.1/34.56-1458-56-38.14-12.38-12-38,34.56-1458-56-38.14-12.38.12-12.38 (31) C2.0/-36-14.56-56.38-14-58-16-58 (25) C2.1/-56-1458-56-38.12-34.58.36-14.58 (30) C2.3/-36-16.58-56.38-34-58.16-12.38 (14) C2.6/-38-14-56-16-12-1258.34-14.58 (58) C2.5/-58-1458-56-36.14-34.58.16-16.38 (26) C3.0/56.34-14.56-56.38-12-58-12-18 (59) C3.2/-58-1458-56-36-12-58-36-58 (29) C3.1/-36-16-56-16-14-58.12-14.38 (18) C3.5/34.58-16-56-38.14-12.38-16-18 (20) C3.4/-38-16-56-16-12-38.12-12.38,-38-16-56-16-12-38-12-38 (37) C3.6/-56-1458-56-38-12-1258-14-78,-56-1458-56-38.12-12.58-14-78 (39)")
		//tunnel(comp, 2.0, 1.8, 20000000)
		anneal(comp, 2000000000)
	}

	class Composition(searchPositions: Vector[SearchPosition]) extends CompositionState(searchPositions)
	{
		def seedFrom(solution: String): Unit =
		{
			val methods = new SolutionOutputter(solution).methods.map{_._2.head}
			courses.zip(methods).foreach{(p)=> p._1.descendMethod(p._2)}
		}

		def energy: Int =
		{
			//val mult = sqrt(searchPositions.foldLeft(1.0){ (n,pos)=> n*pos.branchiness(searchPositions)})
			val depths = searchPositions.map{_.depth}
			val finish = 23*8-depths.sum + 16-2*depths.min
			//val music = /*if (finish==0)*/ searchPositions.map{_.methodMusic}.sum /*else 0*/
			//val music = new FinishedComposition(this).bestCompMusic._1.total
			finish //-music// + (200-mult.toInt)
		}

		/** Mutable, so returns this. */
		def randomMove(): Composition =
		{
			val i = Random.nextInt(searchPositions.size)
			searchPositions(i).randomMove(searchPositions)
			this
		}

		override def toString = energy+": "+super.toString
	}

	def tunnel(comp: Composition, baseK: Double, kMult: Double, maxK: Int) =
	{
		var thisK = baseK
		var current = comp
		while (thisK<maxK)
		{
			val intK = thisK.toInt
			val newComp = anneal(current, intK)
			if (newComp.energy < current.energy)
			{
				println("*** Seeding from new best: "+newComp)
				current = newComp
				thisK = baseK
			}
			else
			{
				thisK*= kMult
			}
		}
	}

	def anneal(comp: Composition, kMax: Int) =
	{
		def temperature(k: Int) = 4*(kMax - k).toDouble/kMax
		//def temperature(k: Int) = 40*(kMax - k).toDouble/kMax

		var k = 0
		var state = comp
		var e = state.energy
		var best = comp
		var bestE = e
		println("Seeding from: "+state)
		var time = System.currentTimeMillis()
		var nMoves:Long = 0
		while (k < kMax)
		{
			val t = temperature(k)
			val newState = state.randomMove()
			val newE = newState.energy
			if (acceptMove(e, newE, t))
			{
				nMoves+= 1
				state = newState
				e = newE
				if (k>0 && k%1000000==0)
				{
					val newT = System.currentTimeMillis()
					val duration = newT-time
					time = newT
					val perfString = " ["+(nMoves*1000/duration)+" moves/s]"
					println("New state at k=" + k + ": " + state + perfString)
				}
				if (newE <= bestE)
				{
					println("New best: "+(new FinishedComposition(newState)))
					best = newState
					bestE = newE
				}
			}
			k+= 1
		}
		best
	}

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


	def solution(searchPositions: Vector[SearchPosition]): Unit =
	{
	}
}

