package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.PN

/** Carries information about a single change from the half-lead (above or below) of a method: the place notation,
	* the treble place (and where the treble was in the last row, as a convenience); and the treble section number.
	* This is the basic data structure describing an element of the work of a method, and we use it for both the
	* parent method (when it is often augmented with a {@link PathRequirement} and extension methods. */
case class PathElement(pn: PN, treblePlace: Int, prevTreblePlace: Int, trebleSection: Int)
{
	def this(p: PathElement, newPN: PN, trebleOffset: Int) =
		this(newPN, p.treblePlace+trebleOffset, p.prevTreblePlace+trebleOffset, p.trebleSection+trebleOffset)
}

/**
 * Carries all the information we need about a single change from the half-lead (above or below) of the
 * parent method. The {@link #path} parameter contains the partial place notation, and the treble position and section.
 * To that we add any requirements on treble and place adjacency which may need to be preserved in extensions.
 * The class is also responsible for testing the requirements.
 *
 * @param path
 */
case class PathRequirement(path: PathElement)
{
	/** A place is adjacent to the treble if either blow of the place is immediately below the treble.
		* Note that we exclude lead and 2nd's place. */
	def trebleAdjacency(path: PathElement): Boolean =
	{
		val lowestTreblePlace = path.treblePlace.min(path.prevTreblePlace)
		lowestTreblePlace>3 && path.pn.isPlace(lowestTreblePlace-1)
	}

	/** This is a list of the sizes of the sets of adjacent places in the row. E.g. pn 347890 would give List(2,4).
		* Note that we ignore adjacent places at lead, i.e. 12. */
	def placeAdjacency(path: PathElement): List[Int] = path.pn.placesAbove(1).consecutives.map(_.nPlaces)

	val trebleAdjacentPlace = trebleAdjacency(path)
	val adjacentPlaceRuns = placeAdjacency(path)

	def hasRequirements = trebleAdjacentPlace || adjacentPlaceRuns.nonEmpty

	/** Check a {@link PathElement} from an extension, assumed to be at a matching point in the lead, to see
		* whether it meets our requirements. The flag values in the {@link ExtensionFlags} instance returned will
		* all be true if it does. */
	def check(path: PathElement): ExtensionFlags =
	{
		val other = PathRequirement(path)
		val trebleAdjacencyOK = trebleAdjacentPlace==other.trebleAdjacentPlace
		val placeAdjacencyOK = adjacentPlaceRuns==other.adjacentPlaceRuns
		ExtensionFlags(trebleAdjacencyOK, placeAdjacencyOK)
	}
}