package net.snowtiger.triples

import net.snowtiger.ringing.{PN, Row}

import scala.collection.mutable

class Node(row: Row, table: CosetTable)
{
  val myRows = mutable.Set[Row]()
  val id = table.incNodes

  populate()

  def nextFromPn(pn: PN) = (pn, findNodeFromRow(row.apply(pn)))
  val nextNodes = Map[PN, Node]() ++ table.pns.map{ nextFromPn }

  def findNodeFromRow(r: Row) =
  {
    if (table.nodes.contains(r))
      table.nodes(r)
    else
      new Node(r, table)
  }

  def foreach(fn: (PN, Node)=>Unit)
  {
    nextNodes.foreach( (p:Tuple2[PN, Node]) => fn(p._1, p._2) )
  }

  def populate()
  {
    def addRow(r: Row)
    {
      val check = table.nodes.get(r)
      if (check.isDefined)
        assert(false)
      table.nodes.put(r, this)
      myRows+= r
    }

    def addLHRots(r: Row)
    {
      var rot = r
      for (i <- 0 until 12)
      {
        addRow(rot)
        rot = rot.permuteBy(table.lhPerm)
      }
    }

    table.allNodes+= this
    addLHRots(row)
    //addLHRots(row.reverse.permuteBy(qlPerm))
    val hlRow = row.permuteBy(table.hlPerm)
    addLHRots(hlRow)
    //addLHRots(hlRow.reverse.permuteBy(qlPerm))
  }

  override def equals(obj: Any) =
  {
    obj match
    {
      case node: Node => myRows==node.myRows
      case _ => false
    }
  }

  override def toString = "Node "+id+" from "+row
}
