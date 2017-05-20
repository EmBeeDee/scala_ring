package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.Row

import scala.collection.mutable.ListBuffer


/**
 * Generates a series of {@link HalfExtension}s for a given extension type (shiftAbove, repeatFrom).
 * Relies on the {@link HalfExtensionBuilder} to supply the cached maps of pre-expanded path elements.
 * Produces the first extension straightaway, including calculation of the extension flags: if these
 * are bad, we can ditch the entire series, but if they are good we can go on to calculate all the higher
 * half-extensions without further worry about path requirements.
 *
 * @author MBD
 */
case class HalfExtensionSeries(heType: HalfExtensionType, builder: HalfExtensionBuilder) extends Ordered[HalfExtensionSeries]
{
	val originalStage = builder.originalStage
	val stageOffset = heType.stageOffset
	val repeatFrom = heType.repeatFrom
	val expansions = builder.allExpansions(heType.shiftAbove)
	val parentPath = builder.parentPath
	val parentLength = parentPath.size

	val copiedPart1 = parentPath.takeWhile(_.trebleSection < repeatFrom + stageOffset)
	val partToExpand1 = parentPath.dropWhile(_.trebleSection < repeatFrom)
	val expandFrom = parentLength-partToExpand1.size
	val changesRepeated = copiedPart1.size+partToExpand1.size-parentLength

	private val GoodFlags = ExtensionFlags(true, true)
	val flags = getFlags()
	val firstExtension = getExtension(1)

	def isGood = flags.isGood

	/** We'll keep the extension with the lowest value, if there are two generating the same PN, so make
		* sure the compareFactor respects this. */
	def compareFactor = stageOffset*10000 - repeatFrom*100 + heType.shiftAbove
	override def compare(that: HalfExtensionSeries) = compareFactor.compareTo(that.compareFactor)

	/** Get the complete list of extensions up to the maximum stage (normally 24). Don't call this unless
		* {@link #isGood} is true.*/
	def getSeries: List[HalfExtension] =
	{
		val xs = ListBuffer[HalfExtension]()
		xs+= firstExtension
		var iteration = 2
		while (originalStage + iteration*stageOffset <= Row.MaxStage)
		{
			val extension = getExtension(iteration)
			xs+= extension
			iteration+= 1
		}
		xs.toList
	}

	/** It is possible to build any half-extension simply by composing the right expanded path elements from the
		* cached list in the {@link HalfExtensionBuilder}. The "expansions" Map in the builder is effectively a function
		* PN(m)(x)(r) = the PN at row r in the parent method, expanded by stage offset x with shiftAbove value m (where
		* x=0 gives the original unexpanded PN). Now if, for example, the parent was a Plain Minor method, the extension
		* was 3CD, and we wanted to find the Royal extension, we would simply compose
		* {@code PN(3)(0)(1...4) + PN(3)(2)(3...4) + PN(3)(4)(3...7)}.
		* This is much quicker than re-applying 3CD to the Major.
		*/
	def getExtension(iteration: Int): HalfExtension =
	{
		val newPath = ListBuffer[PathElement]()
		newPath++= copiedPart1
		var offset = stageOffset
		for (i <- 1 until iteration)
		{
			newPath++= expansions(offset).slice(expandFrom, expandFrom+changesRepeated)
			offset+= stageOffset
		}
		newPath++= expansions(offset).slice(expandFrom, parentLength)
		HalfExtension(originalStage, originalStage+offset, heType, newPath.toList, flags)
	}

	/** The extension flags are based on the first extension only: this is compared to the parent method to
		* make sure all special path requirements (treble and place adjacency) are met.	*/
	def getFlags(): ExtensionFlags =
	{
		val firstExpansion = expansions(stageOffset)
		def check(req: (Int,PathRequirement)): ExtensionFlags =
		{
			val changeToCheck = req._1
			if (changeToCheck<copiedPart1.size)
				GoodFlags
			else
				req._2.check(firstExpansion(changeToCheck))
		}
		val checked = builder.requirementMap.map(check)
		checked.foldLeft(GoodFlags)((a,b)=> a && b)
	}

}