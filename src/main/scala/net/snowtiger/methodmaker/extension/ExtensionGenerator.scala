package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing._

import scala.collection.mutable.ListBuffer

/**
 * The main class responsible for generating and analysis extensions for a given parent method. Currently this
 * must be a symmetric, non-Little treble-dominated method with a standard path (Plain, Treble-Dodging, or simple
 * Alliance with combinations of the two).
 * A list of method libraries should be supplied, so that we can look up matching library methods for any
 * extension found. The extension protocol used is a reasonably close match for that outlined in the current
 * Central Council Decision G: <a href="http://www.methods.org.uk/ccdecs.htm">http://www.methods.org.uk/ccdecs.htm</a>.
 * There are a few minor deviations, generally in order to improve the range and type of extensions we generate:
 * <ol>
 *  <li>(G)B.1 is not checked rigorously: we take an extension to be "indefinite" if it produces a regular series
 *  of extensions to at least stage 24.
 *  <li>We count as valid an extension which produces a short-course method (e.g. 3 leads in Royal), i.e. (G)B.4 is
 *  relaxed.
 *  <li>(G)B.6 "Wherever the parent has a place made adjacent to the path of a hunt bell, this characteristic must be
 *   retained" is not checked for. This condition is only likely to occur in methods with long places
 *   extending through two or more treble sections.
 *	<li>(G)B.7 "Wherever the parent has a place made adjacent to the path of a hunt bell, this characteristic must be
 *	 retained in" is checked for, unless the place is at lead or 2nds (or the equivalent for above work);
 *	 it seems more natural to allow these places stay close to lead in an expanded section (with suitable shiftAbove value),
 *	 although this case does not arise very often since it requires an AB extension below (not currently considered).
 *	 The treble-adjacency rule is taken to apply symmetrically, i.e. if either blow of the place is adjacent to the
 *	 treble then it applies.
 *	<li>(G)B.8 "Wherever the parent has working bells making adjacent places, this characteristic must be retained" is
 *	 checked for, except again for places at lead or 2nd's, because it seems natural to allow 12 to expand to 14,
 *	 producing an extension where the 12 place in the parent is degenerate due to lack of space.
 * </ol>
 * <p>
 * Generation of extensions needs to be done carefully in order to gain good performance. The basic task is to create
 * two lists of "half-extensions", one each for the above and below work, then marry each possible combination together
 * to see if a working full extensions are produced. As an O(nm) algorithm this is potentially quite slow. The
 * following approach is used:
 * <ol>
 *   <li>Process the parent method to pre-calculate as much data as possible. In particular it helps to expand
 *   every place notation in the half-lead by every possible stageOffset and for every shiftAbove value. These
 *   pre-expanded changes can then be used to directly build all following extensions, without needing to carry out
 *   any further processing. The {@link MethodPaths} class is the first step of the process, splitting the parent
 *   method into above and below works, and then delegating to two {@link HalfExtensionBuilder}s to build the tables
 *   or pre-expanded changes (known as {@link PathElement}s). The above work is reversed (both top-to-bottom and,
 *   within each place notation, left-to-right) so that we can process it identically to a below work.
 *	<li>The {@code HalfExtensionBuilder} is able to produce {@link HalfExtensionSeries} instances, which represent
 *	 an entire series of half-extensions of a given type (specific repeatFrom and shiftAbove value). The
 *	 {@code HalfExtensionSeries} class builds the first extension stage immediately, so that any path requirements
 *	 (treble or place adjacency rules) can be checked straightaway, and the extension discarded on failure. If
 *	 however the half-extension is valid, the full series can be built.
 *	<li>The {@link #findExtensions} method in this class is responsible for orchestrating the calls to the classes
 *	 above, generating all valid half-extensions, and also using the {@link HalfExtensionAccumulator} to de-dupe them,
 *	 thus ensuring extensions generating the same place notation are weeded out.
 *	<li>Finally, all remaining half-extensions for above and below works are paired together to create series of
 * 	 {@link FullExtension}s, again by this class. This process is slightly complicated by the fact that the above and
 * 	 below half-extensions may have different stageOffsets: in theory we might have to marry up an above work valid on
 * 	 every fourth stage (ABCD) with a below work valid at every sixth (ABCDEF). For each full extension, we check for
 *	 for validity before adding to an {@link ExtensionSeries}: it must generate the same type of method as the parent
 *	 (e.g Surprise with a PB leadhead).
 * </ol>
 * <p>
 * To use this class, simply pass it a parent {@link NamedMethod}, and a method library list corresponding to the
 * same method type (e.g. the Surprise libraries), and call {@link #findExtensions} to return a list of valid
 * {@link ExtensionSeries} for the method. Each series will have a different type, and each is guaranteed to be different
 * (generating different higher-stage methods).
 * <p>
 * To get more information on the extension series for a method, including which ones are "indefinite", pass the
 * result list back into {@link #analyseExtensions}. This will produce an {@link AnalysedMethod} instance, containing
 * the parent method, all indefinite extension series, and some stats information about the number and type of extensions
 * found, including any anomalies such as where a library method with the same name exists at a higher stage, but doesn't
 * match any of the extensions found.
 * <p>
 * See also {@link SingleMethodExtensionGenerator}, which provides a main() method to call this class for a single
 * parent method and output the results, and {@link LibraryAnalyser}, which does the same for entire method libraries.
 *
 * @author MBD
 */
case class ExtensionGenerator(parentMethod: NamedMethod, methodLibraries: LibraryList)
{
	val originalStage = parentMethod.nbells

	def findExtensions(verbose: Int): List[ExtensionSeries] =
	{
		// First of all work out all possible half-extensions for the given method.
		// To start with, this will only be at the first extension stage, i.e. the half extensions will all have
		// originalStage equal to the stage of our parent method (although they might extend in different stage offsets).
		// The HalfExtensions class will remove duplicates at a given stage offset, so if e.g. 1CD and 1DE are the same,
		// we'll only keep one. However we might still have 1CDEF, which is a match for 1CD applied twice; this will
		// be pruned out in the next step.
		val methodPaths = MethodPaths(parentMethod)
		var aboveExtensions = new HalfExtensionAccumulator()
		var belowExtensions = new HalfExtensionAccumulator()
		for (stageOffset <- 2 to 8 by 2)
			// Start repeating at section 3 (CD type). We could move this back to 1 to start at AB.
			for (repeatFrom <- 3 to originalStage+2-stageOffset)
				for (shiftAbove <- 1 to originalStage-2)
				{
					aboveExtensions+= methodPaths.getHalfExtensionsAbove(stageOffset, repeatFrom, shiftAbove)
					belowExtensions+= methodPaths.getHalfExtensionsBelow(stageOffset, repeatFrom, shiftAbove)
				}

		// For each half-extension, find the half-extension series onto higher stages (up to 24 at the moment).
		// We need to get them all, because we don't know yet which stages the half-extension will be useful at.
		// In the process of doing this, we prune out multiple-stage extensions (e.g. 1CDEF) which produce the
		// same place notation as lower extensions applied multiple times (e.g. 1CDCD).
		val allAboveSeries = aboveExtensions.generateSeries
		val allBelowSeries = belowExtensions.generateSeries

		if (verbose>1)
		{
			println
			println("Above extensions: ")
			for (series <- allAboveSeries)
				println(series.head)
			println

			println("Below extensions: ")
			for (series <- allBelowSeries)
				println(series.head)
			println
		}

		// Finally pair up each above and below extension series, to generate full extension series.
		// Note this is complicated by the fact that the above and below series might have different stage offsets.
		val extensions = ListBuffer[ExtensionSeries]()
		for (aboveSeries <- allAboveSeries)
			for (belowSeries <- allBelowSeries)
			{
				val xs = ListBuffer[EvaluatedExtension]()
				val itAbove = aboveSeries.iterator
				val itBelow = belowSeries.iterator
				while (itAbove.hasNext && itBelow.hasNext)
				{
					var above = itAbove.next()
					var below = itBelow.next()
					while (above.extensionStage!=below.extensionStage && (itAbove.hasNext || itBelow.hasNext))
					{
						while (itAbove.hasNext && above.extensionStage<below.extensionStage)
							above = itAbove.next()
						while (itBelow.hasNext && below.extensionStage<above.extensionStage)
							below = itBelow.next()
					}
					if (above.extensionStage==below.extensionStage)
					{
						val extension = FullExtension.make(parentMethod, above, below)
						if (extension.isDefined)
							xs+= EvaluatedExtension(extension.get, methodLibraries.lookupName(extension.get), 1)
					}
				}
				val series = ExtensionSeries(parentMethod, xs.toList)
				if (series.numberValid>0)
				{
					extensions+= series
					if (verbose>0)
						println(series)
				}
			}
		extensions.toList
	}

	def analyseExtensions(allSeries: List[ExtensionSeries], stats: ExtensionStats): AnalysedMethod =
	{
		val existingExtensions = methodLibraries.findExistingExtensions(parentMethod)

		val indefiniteSeries = allSeries.filter(_.indefinite)
		val analysedMethod = new AnalysedMethod(parentMethod, indefiniteSeries)

		if (indefiniteSeries.size==0)
		{
			stats.noIndefiniteExtensions+= 1
			if (existingExtensions.nonEmpty)
			{
				// Higher-stage libraries do have a method of the same name, even though we haven't found any extensions
				analysedMethod.badExistingExtensions+= 1
				analysedMethod.addComment("Found no extensions, but the libraries have them: " + existingExtensions.mkString(", "))
			}
			else
			{
				analysedMethod.addComment("Found no extensions")
			}
		}
		else
		{
			analysedMethod.hasIndefiniteExtensions+= 1

			val rungAs = indefiniteSeries.flatMap(_.uniqueRungAs)
			val unrung = indefiniteSeries.filter(_.rungAs.isEmpty)

			analysedMethod.addComment("Found "+indefiniteSeries.size+" extension series; ")
			if (rungAs.isEmpty)
				analysedMethod.addComment("none rung")
			else if (unrung.isEmpty)
				analysedMethod.addComment("all rung")
			else
				analysedMethod.addComment(rungAs.size+" extensions rung")

			val allPN = indefiniteSeries.flatMap(_.extensions.map(_.extension.pn)).toSet

			if (existingExtensions.nonEmpty)
			{
				val (existingFound, existingNotFound) = existingExtensions.partition(allPN(_))
				if (existingNotFound.size>0)
				{
					analysedMethod.badExistingExtensions+= existingNotFound.size
					analysedMethod.addComment(", but the libraries have other extensions with this name: "+existingNotFound.mkString(", "))
					if (existingFound.size>0)
						analysedMethod.addComment(" AS WELL as extensions we DID find: "+existingFound.mkString(", "))
				}
				else
				{
					analysedMethod.goodExistingExtensions+= 1
					analysedMethod.addComment(", including the library extensions")
				}
			}
			else
			{
				if (unrung.size==0)
				{
					analysedMethod.higherStageExtensionsAllUsed+= 1
					analysedMethod.addComment(": there are none remaining which could be given the parent name")
				}
				else if (unrung.size==1)
				{
					analysedMethod.unrungUniqueExtensions+= 1
					analysedMethod.addComment(": only one extension remains for the parent name, "+unrung.head.describeExtensions)
				}
				else if (unrung.size==1)
				{
					analysedMethod.unrungUniqueExtensions+= 1
				}
			}
		}
		analysedMethod.updateStats(stats)
		println(analysedMethod)
		analysedMethod
	}

}






