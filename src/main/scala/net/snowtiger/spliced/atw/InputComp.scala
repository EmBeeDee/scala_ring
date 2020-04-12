package net.snowtiger.spliced.atw

import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.util.Random

/**
 * @author mark
 */
case class InputComp(nbells: Int, calling: String, score: Int, rotateTo: String, courses: List[InputCourse])
{
	def this(clone: InputComp, courses: List[InputCourse]) = this(clone.nbells, clone.calling, clone.score, clone.rotateTo, courses)

	/** Filter and optionally transform all the splices in the individual course sets */
	def filterSplices(f: (String)=>Option[String]) = new InputComp(this, courses.map{_.filterSplices(f)})

	def preferShortest = new InputComp(this, courses.map{_.preferShortest})
	def preferLongest = new InputComp(this, courses.map{_.preferLongest})

	def getCompCOs: List[List[CoursingOrder]] =	courses.map{(c)=> c.coList.map{CoursingOrder(_)}}
	def getCompCOsRotated: List[List[CoursingOrder]] =	courses.map{(c)=> genCOs(rotateTo, c.coList)}
	private def genCOs(rotateToRounds: String, cos: List[String]): List[CoursingOrder] =
	{
		val perm = CoursingOrder(rotateToRounds).coPerm(CoursingOrder.Start)
		val permedCOs = cos.map{CoursingOrder(_).permCO(perm)}
		permedCOs
	}
	private def getCOs(cos: List[String]): List[CoursingOrder] = cos.map{CoursingOrder(_)}

	def unflattenByCourse[T](xs: List[T]): List[List[T]] =
	{
		val courseSizes = courses.map{_.shortestSplice}
		assert(courseSizes.sum==xs.size)
		var result = List[List[T]]()
		var i = 0
		for (size <- courseSizes)
		{
			result = xs.slice(i, i+size)::result
			i+= size
		}
		result.reverse
	}

	override def toString = "InputComp("+nbells+", "+score+", \""+rotateTo+"\", List(\n" + courses.map{_.toString}.mkString
}

case class InputCourse(nbells: Int, cos: String, splices: String)
{
	def this(nbells: Int, cos: String, spliceList: List[String]) = this(nbells, cos, spliceList.mkString(","))
	val coList = split(cos).toList
	val spliceList = if (nbells==10) getRoyalSplices else split(splices)
	val spliceSizes = spliceList.map{_.size}.distinct.sorted
	val shortestSplice = spliceSizes.head
	val longestSplice = spliceSizes.last

	def randomSplice = spliceList(Random.nextInt(spliceList.size))

	def filterSplices(f: (String)=>Option[String]) = new InputCourse(nbells, cos, spliceList.flatMap(f(_)))

	def preferShortest = preferLength(shortestSplice)
	def preferLongest = preferLength(longestSplice)
	def preferLength(len: Int) = new InputCourse(nbells, cos, spliceList.filter{_.size==len})

	lazy val courseStructures = spliceList.map{CourseStructure(nbells, _)}
	def leadNumsVisited = courseStructures.map{_.leadsVisited}.toSet

	/** Returns the list of splices which visit the largest number of different LH groups */
	def mostVariedSplices =
	{
		val maxLHgroups = spliceList.map{(s)=> s.toSet.size}.max
		val mostVaried = spliceList.filter{_.toSet.size==maxLHgroups}
		mostVaried
	}

	private def split(s: String) = s.split(',').toList
	/** Exclude short-course methods (no choice for these in libraries) */
	private def getRoyalSplices = split(splices).filter{(s)=> !s.contains("C") && !s.contains("F")}

	override def toString = "InputCourse(\""+cos+"\", \""+splices+"\"),\n"
}

