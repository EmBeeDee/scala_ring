package net.snowtiger.triples

import net.snowtiger.ringing.{PN, Perm, Row}

import scala.collection.mutable;

class CosetTable(val lhPerm: Perm, val hlPerm: Perm, val qlPerm: Perm, val pns: List[PN])
{
  val nodes = mutable.Map[Row, Node]()
  val allNodes = mutable.ListBuffer[Node]()
  var nNodes = 0

  def incNodes = {nNodes+= 1; nNodes}

  val r = Set[Node]()

  println("Table build starts...")
  new Node(Row(7), this)
  println("Table build finished, "+nNodes+" nodes found.")
  checkTable()

  def checkTable()
  {
    var allRows = Set[Row]()
    for (node <- allNodes)
      allRows = allRows++node.myRows
    assert(allRows.size==5040)
    println("all good, "+allRows.size+" rows.")
  }

  def get(row: Row) = nodes(row)

}
