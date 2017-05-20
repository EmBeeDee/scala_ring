package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.{NamedMethod, PN, Row}

import scala.xml.Elem

/**
 * The combination of a specific above and below {@link HalfExtension} to create a full method at the target stage.
 * Note we use a lightweight {@link MethodDetails} class to carry the minimum set of data about the new method, in
 * particular allowing the list of rows in the first lead, as calculated by {@link NamedMethod}, to be garbage-collected.
 * This is important to keep heap space requirements down when generating many extensions (e.g. from a library analysis).
 *
 * @author MBD
 */
case class FullExtension(method: MethodDetails, above: HalfExtension, below: HalfExtension)
{
	val pn = method.pn

	override def toString = method.namePlusClass+" "+method.stage+" "+method.firstLeadHead+" "+method.lhGroup+" "+pn

	def toXML(includeType: Boolean, libraryName: Option[String]): Elem = <Extension>
		<NBells>{method.nbells}</NBells>
		<Stage>{method.stage}</Stage>
		<LeadHead>{method.firstLeadHead}</LeadHead>
		<LHGroup>{method.lhGroup}</LHGroup>
		{if (includeType){
			<Above>{above.extensionType}</Above>
			<Below>{below.extensionType}</Below>
		}}
		{if (libraryName.isDefined) <LibraryName>{libraryName.get}</LibraryName>
		}<PN>{pn}</PN>
	</Extension>
}

object FullExtension
{
	/** For performance, essential to check PB leadhead first, since this is what fails most checks. */
	def hasSameType(parent: NamedMethod, extension: NamedMethod): Boolean =
				parent.isPlainBobType==extension.isPlainBobType &&
				parent.shortClassification==extension.shortClassification &&
				parent.isSymmetric==extension.isSymmetric &&
				parent.isSymmetricFrontToBack==extension.isSymmetricFrontToBack

	/** Need to ditch the NamedMethod to free up the lead rows - saves a lot of heap space. */
	def methodDetails(method: NamedMethod): MethodDetails =
	{
		MethodDetails(method.lead, method.outputPN(), method.nbells, method.stage, method.namePlusClass, method.shortClassification,
			method.lhGroup, method.firstLeadHead, method.isPlainBobType, method.isSymmetric, method.isSymmetricFrontToBack)
	}
	def make(parent: NamedMethod, above: HalfExtension, below: HalfExtension): Option[FullExtension] =
	{
		assert(above.extensionStage == below.extensionStage)
		def extensionType = above.extensionType + "/" + below.extensionType
		val pn = above.pnReversed.zip(below.pn).map((p) => p._1 + p._2)
		val fullLeadPN = pn.tail ++ pn.reverse.tail
		val method = NamedMethod(extensionType, above.extensionStage, fullLeadPN)
		if (hasSameType(parent, method))
			Some(FullExtension(methodDetails(method), above, below))
		else
			None
	}
}

case class MethodDetails(lead: Seq[PN], pn: String, nbells: Int, stage: String,
												 namePlusClass: String, shortClassification: String, lhGroup: String, firstLeadHead: Row,
												 isPlainBobType: Boolean, isSymmetric: Boolean, isSymmetricFrontToBack: Boolean)

