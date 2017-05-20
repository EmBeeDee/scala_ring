package net.snowtiger.methodmaker.extension

/**
 * Simple data structure capturing the type of a half-extension:
 * <ul>
 *   <li>above - true if we were based on the above work, false if below.
 *   <li>stageOffset - how many stages this extension jumps by, e.g. 2, 4, etc.
 *   <li>repeatFrom - which section in the half-lead we start repeating changes from. For below work, a repeatFrom of 1
 *   corresponds to an AB or ABCD (etc) extension; repeatFrom=2 is BC, etc. However for above work, since we work
 *   backwards from the half-lead, we have to reverse the repeatFrom to get the old Central Council nomenclature.
 *   <li>shiftAbove - specifies the minimum place above which we expand place notation; corresponds to the "mode"
 *   in the old Central Council nomenclature.
 * </ul>
 *
 * @author MBD
 */
case class HalfExtensionType(stageOffset: Int, above: Boolean, repeatFrom: Int, shiftAbove: Int)
{
	 def ccExtensionType(originalStage: Int) =
	 {
		 val mode = shiftAbove
		 val ccRepeat = if (above) originalStage+3-stageOffset-repeatFrom else repeatFrom
		 val ccType = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".substring(ccRepeat-1, ccRepeat-1+stageOffset)
		 mode+ccType
	 }

	 def rawExtensionType = repeatFrom+","+shiftAbove

 }