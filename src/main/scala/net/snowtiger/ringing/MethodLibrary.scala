package net.snowtiger.ringing

import net.snowtiger.spliced.MethodAssessor

import scala.io.Source

/**
 * Library of rung methods, expected to be for a single stage and classification, with the ability to look up a method
 * by name (to get place notation) or place notation (to get name).
 *
 * @author MBD
 */
case class MethodLibrary(file: String, methods: List[NamedMethod])
{
	def this(filename: String, nbells: Int) = this(filename, MethodAssessor(nbells).parseMethods(Source.fromFile(filename)))

	/** Map full lead place notation list -> method name and class. */
	val pnToName = methods.map((m) => (m.lead, m.namePlusClass)).toMap

	/** Map method name (no classification or stage) -> place notation string. */
	val nameToMethod = methods.map((m) => (m.name, m)).toMap

	def getMethod(name: String) = nameToMethod(name)

	def pbMethods = methods.filter(_.isPlainBobType)
}

/** A set of libraries for one classification of method, for multiple stages */
case class LibraryList(perStage: Map[Int, MethodLibrary])
{
	def lookupName(stage: Int, pn: Seq[PN]): Option[String] = getLibrary(stage).pnToName.get(pn)

	def findExistingExtensionsPNString(parentMethod: NamedMethod): List[String] =
		perStage.filter((p)=> p._1>parentMethod.nbells).flatMap((p)=> p._2.nameToMethod.get(parentMethod.name)).map(_.outputPN()).toList


	def getLibrary(stage: Int) = perStage.getOrElse(stage, MethodLibrary.EmptyLibrary)
}

object MethodLibrary
{
	val EmptyLibrary = MethodLibrary("Empty", Nil)

	lazy val plainLibraries = makeLibraryList(4, "PlainMinimus.txt", "PlainMinor.txt", "PlainMajor.txt", "PlainRoyal.txt",
		"PlainMaximus.txt", "PlainFourteen.txt", "PlainSixteen.txt")

	lazy val surpriseLibraries = makeLibraryList(6, "SurpriseMinor.txt", "SurpriseMajor.txt", "SurpriseRoyal.txt",
		"SurpriseMax.txt", "SurpriseFourteen.txt", "SurpriseSixteen.txt", "SurpriseEighteen.txt", "SurpriseTwenty.txt")

	lazy val delightLibraries = makeLibraryList(6, "DelightMinor.txt", "DelightMajor.txt", "DelightRoyal.txt",
		"DelightMax.txt")

	protected def makeLibrary(lowestStage: Int, p: (String, Int)) =
	{
		val nbells = p._2 * 2 + lowestStage
		(nbells, new MethodLibrary(p._1, nbells))
	}

	protected def makeLibraryList(lowestStage: Int, fileNames: String*) =
	{
		LibraryList(fileNames.zipWithIndex.map(makeLibrary(lowestStage, _)).toMap)
	}
}