package net.snowtiger.methodmaker.extension

import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, StandardOpenOption}

import net.snowtiger.ringing.{LibraryList, MethodLibrary}

import scala.collection.mutable.ListBuffer
import scala.xml.Elem

/**
 * Analyses extensions for all methods in a given library, starting from a given base stage. It outputs an
 * XML report file.
 * The stage and library to use are currently hardwired in code.
 *
 * @author MBD
 */
object LibraryAnalyser
{
	def main(args: Array[String]): Unit =
	{
		val t = System.currentTimeMillis()
		val lowestStage = 6
		val libraryList = MethodLibrary.surpriseLibraries
		val (stats, analysisList) = checkLibrary(lowestStage, libraryList)
		val dt = System.currentTimeMillis() - t;
		println("Took " + (dt/1000) + "s")

		val (reportName: String, xml: Elem) = produceXmlReport(lowestStage, libraryList, stats, analysisList)

		val xmlBytes = xml.toString().getBytes(Charset.forName("UTF-8"))
		val file = new File(reportName+"_extension_report.xml")
		Files.write(file.toPath, xmlBytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

	}

	def produceXmlReport(lowestStage: Int, libraryList: LibraryList, stats: ExtensionStats, analysisList: List[AnalysedMethod]): (String, Elem) =
	{
		val sortedList = analysisList.sorted
		val (bad, rest1) = sortedList.span(_.badExistingExtensions > 0)
		val (allUsed, rest2) = rest1.span(_.higherStageExtensionsAllUsed > 0)
		val (unrung, rest3) = rest2.span(_.unrungUniqueExtensions > 0)
		val (good, rest4) = rest3.span(_.goodExistingExtensions > 0)
		val (exist, none) = rest4.span(_.hasIndefiniteExtensions > 0)

		val reportName = libraryList.getLibrary(lowestStage).file.takeWhile(_ != '.')

		val xml = <AnalysedMethods>
			<ReportName>
				{reportName}
			</ReportName>{stats.toXML}<BadExistingList>
				{bad.map(_.toXML)}
			</BadExistingList>
			<AllUsedList>
				{allUsed.map(_.toXML)}
			</AllUsedList>
			<UniqueUnrungList>
				{unrung.map(_.toXML)}
			</UniqueUnrungList>
			<GoodExistingList>
				{good.map(_.toXML)}
			</GoodExistingList>
			<HasExtensionsList>
				{exist.map(_.toXML)}
			</HasExtensionsList>
			<NoExtensionsList>
				{none.map(_.toXML)}
			</NoExtensionsList>
		</AnalysedMethods>
		(reportName, xml)
	}

	def checkLibrary(stage: Int, libraries: LibraryList): (ExtensionStats, List[AnalysedMethod]) =
	{
		val baseLibrary = libraries.getLibrary(stage)
		val results = ListBuffer[AnalysedMethod]()

		println()
		val stats = new ExtensionStats
		for (baseMethod <- baseLibrary.pbMethods; if baseMethod.isSymmetric)
		{
			val ef = ExtensionGenerator(baseMethod, libraries)
			val extensionSeries = ef.findExtensions(0)
			val analysedMethod = ef.analyseExtensions(extensionSeries, stats)
			results+= analysedMethod
		}
		println()
		stats.print()
		(stats, results.toList)
	}

}