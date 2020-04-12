package net.snowtiger.spliced.atw.construct

/**
 * @author mark
 */

class TreblePath
{
	/** Number of changes in each section - allows Plain, TD and various Alliance methods in between */
	val SectionSizes = List(4,4,4,4)
	val TreblePath = (0 to 3).toList.map{(n)=> List.fill(SectionSizes(n)/2)(List(n*2,n*2+1)).flatten}.flatten
	/** Number of changes in halflead, given 0..4 sections, where 0 sections = 1 leadhead change */
	val SectionLengths = 1::(SectionSizes.scanLeft(0)(_+_).tail)
	val HalfLeadLength = SectionLengths(4)
	val LeadLength = HalfLeadLength*2
	val CompLength = LeadLength*7*23
	/** First value is 0 for leadhead, then (for TD) 3, 7, 11, 15 */
	val SectionEndOffsets = SectionLengths.map{_-1}

}