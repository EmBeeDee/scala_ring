package net.snowtiger.puzzles

import net.snowtiger.ringing.Row

import scala.io.Source

/**
 * @author mark
 */

object MethodAnagrams
{
	val stageNames = Row.Stages.slice(2,12).toSet
	val stageNamesSorted = stageNames.map((s)=> (s, s.sorted))
	val classNames = Set("Little","Bob","Place","Alliance","Treble","Delight","Surprise")
	val classNamesSorted = classNames.map((s)=> (s, s.sorted))

	/** Only method names with letters, space and apostrophe, and only for stages 3-12 */
	def isGoodName(name: String) = stageNames.exists(name.endsWith) && name.forall((c)=> c.isLetter || c==' ' || c=='\'')

	def stripAllButLetters(name: String) = name.filter(_.isLetter)

	/** Both strings must be sorted */
	def isSubAnagram(name: String, sub: String): Boolean =
	{
		val it = sub.iterator
		var remaining = name
		var exhausted = false
		while (it.hasNext && !exhausted)
		{
			val c = it.next()
			remaining = remaining.dropWhile(_!=c)
			if (remaining.isEmpty)
				exhausted = true
			else
				remaining = remaining.tail
		}
		!exhausted
	}

	def stagesWithin(name: String): Set[String] = wordsWithin(name, stageNamesSorted)
	def classesWithin(name: String): Set[String] = wordsWithin(name, classNamesSorted)
	def wordsWithin(name: String, words: Set[(String,String)]): Set[String] = words.filter{(ss)=> isSubAnagram(name, ss._2)}.map(_._1)

	def main(args: Array[String]): Unit =
	{
		val allMethods = Source.fromFile("d:\\ringing\\AllMethods.txt", "UTF8").getLines().toSet.filter(isGoodName).map(stripAllButLetters).toList.filter(_.size<=125)
		val sorted = allMethods.map((s)=> (s,s.sorted)).toMap
		var stages = sorted.mapValues(stagesWithin)
		val classes = sorted.mapValues(classesWithin)

		for (method <- allMethods.sortBy((s)=> -classes(s).size*100-stages(s).size*1000+s.size).take(1000))
			println(stages(method).size+" "+classes(method).size+" "+method.size+" "+method+"   "+stages(method).mkString(" ")+"   "+classes(method).mkString(" "))

	}

}