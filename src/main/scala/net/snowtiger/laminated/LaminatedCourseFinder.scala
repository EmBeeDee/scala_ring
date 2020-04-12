package net.snowtiger.laminated

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.StandardMethods
import net.snowtiger.spliced.composition.Major
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.coursingorder.CoursingOrder
import net.snowtiger.spliced.tables.Node

/**
 * @author mark
 */

object LaminatedCourseFinder extends StandardMethods
{
	val methods = List(yorkshire, superlative, cambridge, london, lessness, bristol)
	//val methods = List(yorkshire, superlative, cornwall, lessness, bristol)
	//val methods = List(yorkshire, superlative, bristol)
	val callingPositions = List(Major.Home)
	//val coursingOrders = CoursingOrder.all
	val coursingOrders = CoursingOrder.positive

	val music = Array(new StandardMajorMusic(), new Music56Rollup(), new MusicLB(5), new Music65Rollup(), new MusicQueens())

	val nodes: Map[NamedMethod, List[Node]] =
		methods.map((method)=> (method, LaminatedNodeBuilder(method, callingPositions, coursingOrders, music).nodes)).toMap

	val allNodes = nodes.values.flatten.toSet

	val falseNodes: Map[Node, Set[Node]] =
		allNodes.map((node)=> (node, allNodes.filter(!_.allRows.intersect(node.allRows).isEmpty))).toMap

	def main(args: Array[String]): Unit =
	{
		println("Searching...")
		search(Nil, Map(), nodes, List(superlative))
	}

	var MINCOURSES = 16
	val nMethods = methods.size

	def search(nodesFound: List[Node], methodsFound: Map[NamedMethod,Int], nodesLeft: Map[NamedMethod, List[Node]], methodOrder: List[NamedMethod]): Unit =
	{
		val methList = if (methodOrder.isEmpty) nodesLeft.keys.toList.sortBy(nodesLeft(_).size) else methodOrder
		val method = methList.head
		nodesLeft(method) match
		{
			case (node :: remaining) =>
			{
				val falseList = falseNodes(node)
				val newNodesLeft = nodesLeft.mapValues(_.filter(!falseList(_))).toMap
				val newMethodsFound = methodsFound + (method->(1+methodsFound.getOrElse(method,0)))
				search(node::nodesFound, newMethodsFound, newNodesLeft, methList.tail)
				// Must have the plain course, so don't search other courses for the first node.
				// TODO actually not all methods start with the plain course (e.g Superlative 65432 is more musical) hence we might miss the PC anyway...
				if (nodesFound.size>0)
					search(nodesFound, methodsFound, nodesLeft + (method -> remaining), methList)
			}
			case Nil =>
			{
				val newNodesLeft = nodesLeft-method
				if (newNodesLeft.isEmpty)
				{
					if (nodesFound.size>=MINCOURSES && methodsFound.size==nMethods)
						print(nodesFound)
				}
				else if (methodsFound.getOrElse(method,0)>=2 && nodesFound.size+newNodesLeft.size*2>=MINCOURSES)
					search(nodesFound, methodsFound, newNodesLeft, methList.tail)
			}
		}
	}

	def print(nodes: List[Node]): Unit =
	{
		val courses = nodes.reverse.map((n)=> n.startLH.coursingOrder()+" "+n.methodString.head)
		val music = nodes.map(_.music).reduceRight((a,b)=> a+b)
		println(nodes.size+" "+courses.mkString(" ")+" "+music)
	}
}