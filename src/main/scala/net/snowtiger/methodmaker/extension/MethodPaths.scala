package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.{NamedMethod, PN}

/**
 * The first stage of the production of extensions from a parent method: we build two <em>path</em> data structures,
 * one each for the above and below works of the parent method. A path specifies the treble positions and sections,
 * and partial PN for a half-lead of work. We normalise the above work by reversing it top-to-bottom and left-to-right,
 * so that it looks like a below work. We also build a list of {@link PathRequirement}s, which indicate where in the
 * original method there might be extra requirements imposed on extensions, for instance where adjacent places are made,
 * or where there is a place adjacent to the treble. The path lists always include the leadhead and halflead changes,
 * so are one larger than the half-lead length.
 * <p>
 * The results of this work are two {@link HalfExtensionBuilder} instances, one each for the above and below works.
 * These are responsible for carrying out further preparatory work on the parent method paths, including caching
 * any additional data we need for extension generation. The builder instance then supply, via the {@link #getHalfExtensionsAbove}
 * and {@link #getHalfExtensionsBelow} methods in this class, two {@link HalfExtensionSeries}, which contain all the
 * half-extensions up to the maximum stage.
 *
 * @author MBD
 */
case class MethodPaths(method: NamedMethod)
{
	val originalStage = method.nbells

	val trebleStartPos = 1
	val halfLeadPN = method.lead(method.leadLength / 2 - 1)
	val halfLeadTreblePos = method.firstLead(method.leadLength / 2).placeOf(1)
	val above = method.leadheadPN.placesAbove(trebleStartPos) +: (method.workAbove :+ halfLeadPN.placesAbove(halfLeadTreblePos - 1))
	val below = PN("" + trebleStartPos) +: (method.workBelow :+ halfLeadPN.placesBelow(halfLeadTreblePos))
	val aboveReversed = PN.reverse(above, originalStage)

	val treblePath = method.treblePositions.take(method.leadLength / 2 + 1)
	val treblePathReversed = (trebleStartPos :: method.treblePositions.take(method.leadLength / 2)).reverse.map(originalStage + 1 - _)
	val trebleSections = treblePathToSections(treblePath)
	val trebleSectionsReversed = treblePathToSections(treblePathReversed)

	val pathAbove = makePath(aboveReversed, treblePathReversed, trebleSectionsReversed)
	val pathBelow = makePath(below, treblePath, trebleSections)

	val aboveBuilder = HalfExtensionBuilder(originalStage, pathAbove)
	val belowBuilder = HalfExtensionBuilder(originalStage, pathBelow)

	/** Get the {@link HalfExtensionSeries} for the above work. */
	def getHalfExtensionsAbove(stageOffset: Int, repeatFrom: Int, shiftAbove: Int): HalfExtensionSeries =
		getHalfExtensions(stageOffset, true, repeatFrom, shiftAbove, aboveBuilder)

	/** Get the {@link HalfExtensionSeries} for the below work. */
	def getHalfExtensionsBelow(stageOffset: Int, repeatFrom: Int, shiftAbove: Int): HalfExtensionSeries =
		getHalfExtensions(stageOffset, false, repeatFrom, shiftAbove, belowBuilder)

	private def getHalfExtensions(stageOffset: Int, above: Boolean, repeatFrom: Int, shiftAbove: Int, builder: HalfExtensionBuilder) =
	{
		val heType = HalfExtensionType(stageOffset, above, repeatFrom, shiftAbove)
		val halfExtensionSeries = builder.getSeries(heType)
		halfExtensionSeries
	}

	private def makePath(pn: Seq[PN], treblePath: List[Int], trebleSections: List[Int]): List[PathRequirement] =
	{
		val treblePathShifted = treblePath.head::treblePath
		val basicPath = pn.zip(treblePath).zip(treblePathShifted.zip(trebleSections)).map{(p)=> PathElement(p._1._1, p._1._2, p._2._1, p._2._2)}
		basicPath.map(PathRequirement).toList
	}

	/** Number each change in the half-lead with the treble section number, starting at 1 for the leadhead/halflead.
		* For instance, Surprise Minor gives 1222344456667. */
	private def treblePathToSections(path: List[Int]) =
	{
		// Will add section 1 (treble full lead) on at the end
		var section = 2
		def toSection(places: List[Int]): Int =
		{
			val min = places.min
			val max = places.max
			// If the treble makes a place, or it moves from an even to an odd place, count a new section
			if (min==max || max%2==1)
			{
				section+= 2
				section-1
			}
			else
				section
		}
		val sections = path.sliding(2).map(toSection).toList
		1::sections
	}
}



