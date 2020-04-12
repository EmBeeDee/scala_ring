package net.snowtiger.spliced.search

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.tables._

import scala.io.Source

/**
 * @author mark
 */

trait SearchDefinitionBase extends SearchDefinition
{
	lazy val stage = new Stage(getCallingMethod.nbells)
	override lazy val calls = List(stage.Bob, stage.Single)

	override lazy val musicDefs =
	{
		if (getCallingMethod.nbells>8)
			Array(new StandardMaxMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup())
		else
			Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup())
	}

	private var compPlan: Option[CompositionPlan] = None
	def makePlan = new CompositionPlan(this)
	final def getCompPlan = {if (compPlan.isEmpty) compPlan = Some(makePlan); compPlan.get}


	override def getExcludedLeads: Iterable[Lead] = Nil

	val seed: Option[String] = None
	lazy val seedProvider =
		seed match
		{
			case None => new OriginalCompSeedMayBeFalse(this)
			case Some(str) if isCompStr(str) => new StringSeed(this, str)
			case Some(file) => new FileSeed(this, file)
		}

	def isCompStr(str: String) = str.length>100

	def prettyPrint() =
	{
		val printer = new PrettyPrinter(this)
		val str = seed.get
		val source = if (isCompStr(str)) Source.fromString(str) else Source.fromFile(str)

		printer.print(source)
	}

	def assessMethods() =
	{
		val music = musicDefs(0)
		val nbells = methods(0).nbells
		val positiveCHs = Row.genPositiveTenorsTogetherCourseHeads(nbells)
		var combined = Map[Row, Int]()
		for (method <- methods)
		{
			println("Best courses for "+method.description+":")
			val bestCourses = method.findBestCourses(positiveCHs, music).filter{_._1>0}
			for ( (score, ch) <- bestCourses)
			{
				println(score+", "+ch)
				combined+= Tuple2(ch, score+combined.getOrElse(ch,0) )
			}
			println()
		}
		println("Combined:")
		for ( (ch, score) <- combined.toList.sortBy(_._2).reverse)
			println(score+", "+ch)

	}

}