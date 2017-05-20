package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.Row

/**
 * Helper class responsible for building {@link HalfExtensionSeries}, given a list of {@link PathRequirement}s
 * representing the above or below work of the parent method. It is a critical component in the efficiency of
 * the generation process, caching all the "expanded" path elements of the parent method, for every possible
 * {@code shiftAbove} and {@code stageOffset} value. An expanded element is basically one with the place notation
 * expanded (respecting the shift), and the treble position and sections also moved up, all by the value of the
 * stage offset. We process offsets high enough so that we can supply expanded elements for the highest stage
 * (typically 24) directly from the parent method stage (e.g. Minor). Any half-extensions can then be built
 * simply by appending the right sequence of unexpanded and expanded path elements, without any further processing
 * on them.
 * <p>
 * As an additional efficiency, this class also turns the list of special path requirements into a Map covering
 * only the changes (in the parent) that have such requirements (e.g. treble or place adjacency). This also assists
 * in the performance of the {@code HalfExtensionSeries} generation.
 *
 * @author MBD
 */
case class HalfExtensionBuilder(originalStage: Int, pathRequirements: List[PathRequirement])
{
	val parentPath = pathRequirements.map(_.path).toArray

	/** Since most changes in the lead won't have special requirements, we convert them into a Map
		* from the change number to the requirement - a sparser data structure. */
	val requirementMap = pathRequirements.zipWithIndex.filter(_._1.hasRequirements).map(_.swap).toMap

	/** The main cache of expanded path elements: a Map shiftAbove -> stageOffset -> array of expanded path elements.
		* We guarantee we'll have an expansion for every stage and every shiftAbove value. */
	val allExpansions = genExpansions()

	/** The result of our work - a {@link HalfExtensionSeries} built using our cached expansions. */
	def getSeries(heType: HalfExtensionType) = HalfExtensionSeries(heType, this)

	/** Carries out the critical work of expanding all the parent path elements, for every possible shift
		* and stage-offset. */
	private def genExpansions(): Map[Int,Map[Int,Seq[PathElement]]] =
	{
		def gen(shiftAbove: Int) = (shiftAbove, genExpansions(shiftAbove))
		(1 to originalStage-2).map(gen).toMap
	}

	private def genExpansions(shiftAbove: Int): Map[Int,Seq[PathElement]] =
	{
		def gen(stageOffset: Int) = (stageOffset, genExpansions(shiftAbove, stageOffset))
		(2 to Row.MaxStage-originalStage by 2).map(gen).toMap
	}

	private def genExpansions(shiftAbove: Int, stageOffset: Int): Seq[PathElement] =
	{
		def expand(p: PathElement) = new PathElement(p, p.pn.shiftUpAbove(shiftAbove, stageOffset), stageOffset)
		parentPath.map(expand)
	}

}
