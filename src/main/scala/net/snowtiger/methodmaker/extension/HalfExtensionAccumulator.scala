package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.PN

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * Responsible for accumulating all the {@link HalfExtension}s for a given parent method (for either above or below
 * work), weeding out duplicates which generate the same partial place notations. The de-duplication must be done
 * in two phases:
 * <ol>
 *   <li>We look at all the first-step extensions first. Where two first extensions have the same place notation,
 *   we keep the one with the lowest compareTo. During this phase we're not able to compare extensions with different
 *   stageOffsets, i.e. if the parent is Minor then we will store all 2-stage extensions unique at the Major stage,
 *   as well all the 4-stage extensions unique at the Royal stage, but don't yet compare between the two.
 *   <li>Once all the first-step extension have been done, we generate the full half-extension series for each
 *   remaining instance, starting with the smaller extensions. During this phase, if the first extension from a series
 *   with a higher stage offset matches the place notation already generated in the series of a smaller extension
 *   (e.g. ABCD produces the same results as ABAB) then we leave out the bigger extension.
 * </ol>
 *
 * @author MBD
 */
class HalfExtensionAccumulator
{
	 val extensionList = mutable.LinkedHashMap[Seq[PN],HalfExtensionSeries]()

	 /** Add a new HalfExtensionSeries to the list, but only if it's good enough, and only if there isn't another
		 * series already present which generates the same PN for its first extension, and has a lower compareTo. */
	 def +=(heSeries: HalfExtensionSeries) =
	 {
		 if (heSeries.isGood)
		 {
			 val key = heSeries.firstExtension.pn
			 extensionList.get(key) match
			 {
				 case None => extensionList.put(key, heSeries)
				 case Some(other) => if (heSeries<other) extensionList.put(key, heSeries)
			 }
		 }
	 }

	/**
	 * Generate all unique series of half-extensions. We weed out extensions with higher stageOffsets which produce
	 * duplicate results to a smaller extension applied multiple times (e.g. if ABCD = ABAB, we keep AB and omit ABCD).
	 * @return
	 */
	def generateSeries: List[List[HalfExtension]] =
	{
		val allExtensions = mutable.Set[Seq[PN]]()
		val results = ListBuffer[List[HalfExtension]]()
		for (extension <- extensionList.values)
			if (!allExtensions(extension.firstExtension.pn))
			{
				val series = extension.getSeries
				results += series
				for (e <- series)
					allExtensions += e.pn
			}
		results.toList
	}
 }