package net.snowtiger.spliced

import java.io.{OutputStream, PrintStream}

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.search.{ClonedSearchDef, SearchDefinition, StochasticTunnellingSearch}

/**
 * @author mark
 */

class MultiMethodGenerator(val baseDef: SearchDefinition, extraMethods: List[NamedMethod])
{
	def multi(maxK: Int)
	{
		val baseMethods = baseDef.methods

		println(extraMethods.size+" methods to try over "+baseMethods.map{_.name}.mkString(", "))

		for (method <- extraMethods)
		{
			print("Trying "+method.name)
			val t = System.currentTimeMillis()
			val mScore = (1+baseMethods.size)*1000000

			val searchDef = new ClonedSearchDef(baseDef){
				override val logger = new PrintStream(new NullOutputStream())
				override val methods = method::baseDef.methods
			}

			val maxTime = maxK*12

			val searcher = new StochasticTunnellingSearch(searchDef, maxK, maxTime)
			{
				override val nCompsToKeep: Int = 1
			}
			val comps = searcher.varyFromSeed()
			print(" "+(System.currentTimeMillis()-t)/1000+"s")
			if (comps.size==0)
				println(" NONE!")
			else
			{
				print("\r")
				val comp = comps.head
				println((comp.score-mScore)+", *="+comp.methodCounts(List(method)).head+", "+method.name+", "+comp)
			}
		}
	}
}

class NullOutputStream extends OutputStream
{
	def write(b: Int)
	{
		// no-op
	}
}
