package net.snowtiger.triples

import net.snowtiger.ringing.{Method, Row}

import scala.collection.mutable;

/**
 * Created with IntelliJ IDEA.
 * User: Mark
 * Date: 17/08/12
 * Time: 17:47
 * To change this template use File | Settings | File Templates.
 */

class HelixChecker(method: Method)
{
  def check()
  {
    val numbered = method.fullCourse.zipWithIndex
    val map = mutable.Map[Row,Int]()
    for (rowN <- numbered)
      if (map.contains(rowN._1))
      {
        val i = map(rowN._1)
        val j = rowN._2
        println("Collision: "+rowN._1+" - "+i+" ("+(i%210)+"), "+j+" ("+(j%210)+")")
      }
      else
        map+= rowN._1->rowN._2
  }
}
