package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.{PN, Row}

/**
 * Represents the expansion of a half-work (above or below) to a specified extension stage.
 * It gives us the extension stage, the extension type, and the partial place notation of the extension, as a list
 * of {@link PathElement}s.
 *
 * @author MBD
 */
case class HalfExtension(originalStage: Int, extensionStage: Int, heType: HalfExtensionType, path: Seq[PathElement], flags: ExtensionFlags)
{
	def completePn(pn: PN, path: PathElement): PN =
		if (path.trebleSection%2==1) pn + PN(Array(path.trebleSection.min(extensionStage-1))) else pn

	def stageOffset = heType.stageOffset
	def repeatFrom = heType.repeatFrom
	def shiftAbove = heType.shiftAbove
	def above = heType.above

	val extensionType = heType.ccExtensionType(originalStage)

	// These properties are not normally calculated, so are marked as lazy.
	// In theory the lastRow of the "completed" place notation has the bells in the order they passed the treble;
	// it may be possible to combine this with the equivalent permutation from the other work for a quick determination
	// of the half-lead of a full extension.
	lazy val pn: Seq[PN] = path.map(_.pn).toArray[PN]
	lazy val pnCompletedAbove = pn.zip(path).map( (p)=> completePn(p._1, p._2) )
	lazy val lastRow = PN.generateLastRow(Row(extensionStage), pnCompletedAbove.tail)
	lazy val perm = lastRow.toPerm
	lazy val reversePerm = perm.inverse

	lazy val pnReversed = PN.reverse(pn, extensionStage)

	def outputPN = PN.output(if (above) pnReversed else pn)
	override def toString: String = lastRow.toString+" "+extensionType+" "+outputPN
}


/** Just tell us whether the special path requirements have been met or not. */
case class ExtensionFlags(trebleAdjacencyPreserved: Boolean, placeAdjacencyPreserved: Boolean)
{
	def &&(other: ExtensionFlags) =
		ExtensionFlags(trebleAdjacencyPreserved&&other.trebleAdjacencyPreserved, placeAdjacencyPreserved&&other.placeAdjacencyPreserved)

	def isGood = trebleAdjacencyPreserved && placeAdjacencyPreserved
}
