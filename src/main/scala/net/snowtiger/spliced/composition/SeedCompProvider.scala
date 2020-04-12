package net.snowtiger.spliced.composition

import net.snowtiger.spliced.search.{SearchBase, SearchDefinition}
import net.snowtiger.spliced.tables.Node

import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * @author mark
 */

trait SeedCompProvider
{
	def getSeeds(searcher: SearchBase): TreeSet[Composition]
}

class VariationSeed(variations: TreeSet[Composition]) extends SeedCompProvider
{
	def getSeeds(searcher: SearchBase) = variations
}

class FileSeed(searchDef: SearchDefinition, val filename: String) extends SeedCompProvider
{
	def getSeeds(searcher: SearchBase) =
	{
		val loader = new TableCompositionLoader(searcher.tables, searchDef.methods)
		searchDef.logger.println("Loading seed compositions from file "+filename)
		loader.loadFromFile(Source.fromFile(filename))
	}
}

class StringSeed(searchDef: SearchDefinition, val comp: String) extends SeedCompProvider
{
	def getSeeds(searcher: SearchBase) =
	{
		val loader = new TableCompositionLoader(searcher.tables, searchDef.methods)
		searchDef.logger.println("Loading seed composition  "+comp)
		loader.loadFromFile(Source.fromString(comp))
	}
}

class OriginalCompSeed(searchDef: SearchDefinition) extends SeedCompProvider
{
	def getSeeds(searcher: SearchBase) =
	{
		val comp = searchDef.getCompPlan.buildSingleMethodComp(searcher.tables, true)
		searcher.out.println("Seeding from single-method comp: "+comp.toString)
		TreeSet(comp)
	}
}

class OriginalCompSeedMayBeFalse(searchDef: SearchDefinition) extends SeedCompProvider
{
	def getSeeds(searcher: SearchBase) =
	{
		val comp = searchDef.getCompPlan.buildSingleMethodComp(searcher.tables, false)
		// Can't output composition since it may contains nodes not in the table!
		TreeSet(comp)
	}
}

/**
 * Used with the {@link net.snowtiger.spliced.search.HelperCompositionSearch} - the actual seed is the
 * original base comp, but access is provided to the "helper" composition - just a list of nodes, read from the first
 * comp in the seed file.
 *
 * @param searchDef
 * @param filename
 */
class HelperCompSeed(searchDef: SearchDefinition, val filename: String) extends OriginalCompSeed(searchDef)
{
	def getHelperSeed(helperPlan: CompositionPlan): List[Node] =
	{
		val loader = new CompositionLoader(helperPlan, searchDef.methods)
		searchDef.logger.println("Loading helper composition from file "+filename)
		loader.loadNodesFromFile(Source.fromFile(filename)).next()
	}
}
