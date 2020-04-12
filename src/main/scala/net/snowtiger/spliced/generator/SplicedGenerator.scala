package net.snowtiger.spliced.generator

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition.{Composition, VariationSeed}
import net.snowtiger.spliced.search._

import scala.collection.immutable.TreeSet

/**
 * @author mark
 */

abstract class SplicedGenerator extends SearchFactory
{
	def generate()

	def main(args: Array[String])
	{
		generate()
	}

	def varyComps(baseDef: SearchDefinition, newMethods: List[NamedMethod], variations: TreeSet[Composition]) =
	{
		val newDef = new ClonedSearchDef(baseDef){
			override val methods = newMethods
			override val seedProvider = new VariationSeed(variations)
		}
		greedy(newDef)
	}
}