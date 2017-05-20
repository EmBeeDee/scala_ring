package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.{NamedMethod, Row}

import scala.xml.Elem

/**
 * For a given full extension type, holds the list of all the valid extensions up to the stage limit.
 * It is responsible for checking whether an extension series is "practically indefinite".
 *
 * @author MBD
 */
case class ExtensionSeries(parentMethod: NamedMethod, extensions: List[EvaluatedExtension])
{
	val numberValid = extensions.size
	val validStages: List[Int] = parentMethod.nbells::extensions.map(_.extension.method.nbells)

	def ::(e: EvaluatedExtension) = ExtensionSeries(parentMethod, e::extensions)

	lazy val stageIncrements = if (validStages.size<=1) Nil else validStages.sliding(2).map((xs)=> xs.tail.head-xs.head).toList
	lazy val uniqueRungAs = extensions.flatMap(_.libraryName).toSet
	lazy val rungAs = extensions.flatMap(_.nameAndStage)

	/** A series is indefinite if it generates at least two extensions, these are spaced at reasonably equal stage increments,
		* and they do not run out before the stage limit. */
	def indefinite =
	{
		val minIncrement = stageIncrements.min
		val maxIncrement = stageIncrements.max
		stageIncrements.size > 1 && maxIncrement/minIncrement<3 &&
				validStages.last+maxIncrement > Row.MaxStage
	}

	def describeHigherExtensions =
	{
		val s = new StringBuilder()
		if (numberValid<=1)
			s.append("no higher extensions")
		else
		{
			//"next = "+extensions.tail.head+", "+
			s.append(numberValid)
			s.append(" higher extensions, to ")
			s.append(extensions.last.extension.method.stage)
			if (indefinite)
				s.append(" (practically indefinite)")
			if (rungAs.nonEmpty)
					s.append(" rung as ").append(rungAs.mkString(", "))
		}
		s.toString()
	}

	def describeExtensions = extensions.head+" "+describeHigherExtensions

	override def toString: String = parentMethod.namePlusClass+" "+parentMethod.stage+" " + (extensions match
		{
			case Nil => "has no extensions"
			case first::rest => "extends to "+first+" "+describeHigherExtensions
		})

	def toXML(id: String): Elem = <ExtensionSeries>
		<Id>{id}</Id>
		{if (extensions.size>0) {
			val ext = extensions.head.extension
			<Above>{ext.above.extensionType}</Above>
			<Below>{ext.below.extensionType}</Below>
			<Indefinite>{indefinite}</Indefinite>
			<NExtensions>{extensions.size}</NExtensions>
		}}
		{if (rungAs.size>0) <RungAs>{rungAs.mkString(", ")}</RungAs>}
		{extensions.map(_.toXML(false))}
	</ExtensionSeries>
}

