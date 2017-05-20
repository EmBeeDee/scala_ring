package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.NamedMethod

/**
 * A parent method together with all its indefinite extension series, plus "stats" information detailing number of rung
 * extensions and highlighting any anomalies - see {@link ExtensionStats}.
 *
 * @author MBD
 */
class AnalysedMethod(val parentMethod: NamedMethod, indefiniteExtensions: List[ExtensionSeries]) extends ExtensionStats with Ordered[AnalysedMethod]
 {
	 val id = ""+AnalysedMethod.nextId()
	 var comments = ""

	 def addComment(s: String): Unit =
	 {
		 comments = comments+s
	 }

	 def updateStats(stats: ExtensionStats): Unit =
	 {
		 stats.noIndefiniteExtensions+= noIndefiniteExtensions
		 stats.hasIndefiniteExtensions+= hasIndefiniteExtensions
		 stats.badExistingExtensions+= badExistingExtensions
		 stats.higherStageExtensionsAllUsed+= higherStageExtensionsAllUsed
		 stats.goodExistingExtensions+= goodExistingExtensions
		 stats.unrungUniqueExtensions+= unrungUniqueExtensions
	 }

	 /** Put exceptions first - bad existing extensions, and extensions all used - then the unrung unique extensions,
		 * followed by all methods with extensions, finishing with those with no extensions.
		 */
	 private def sortScore =
	 {
		 def add(score: Int, property: Int) = score*10 + property.min(1)
		 var score = 0
		 score = add(score, badExistingExtensions)
		 score = add(score, higherStageExtensionsAllUsed)
		 score = add(score, unrungUniqueExtensions)
		 score = add(score, goodExistingExtensions)
		 score = add(score, hasIndefiniteExtensions)
		 -score
	 }

	 /** Compare by sortScore, or if that equal, method name */
	 override def compare(that: AnalysedMethod) =
	 {
		 val ourScore = sortScore
		 val otherScore = that.sortScore
		 if (ourScore==otherScore)
			 parentMethod.name.compare(that.parentMethod.name)
		 else
			 ourScore.compareTo(otherScore)
	 }

	 override def toString = parentMethod.namePlusClass+" "+parentMethod.stage+": "+comments

	 override def toXML = <Method>
		 {parentMethod.toXML}
		 <ExtensionReport>
			 <Id>{id}</Id>
			 <Comments>{comments}</Comments>
			 <NExtensionSeries>{indefiniteExtensions.size}</NExtensionSeries>
			 {if (indefiniteExtensions.size>0) {
				 <ExtensionRung>{goodExistingExtensions>0}</ExtensionRung>
				 <BadExistingExtensions>{badExistingExtensions>0}</BadExistingExtensions>
				 <ExtensionsAllTaken>{higherStageExtensionsAllUsed>0}</ExtensionsAllTaken>
				 <UnrungUniqueExtension>{unrungUniqueExtensions>0}</UnrungUniqueExtension>
			 }}
			 {indefiniteExtensions.zipWithIndex.map((p)=> p._1.toXML(id+"."+p._2))}
		 </ExtensionReport>
	 </Method>
 }

object AnalysedMethod
{
	 var id = 0
	 def nextId() = {id+=1; id}
 }