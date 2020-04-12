package net.snowtiger.doublehelix

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.PN

import scala.io.Source

/**
 * @author Mark
 */

object HelixOutputParser extends HelixoidHelper(GoodPn(8).noConsec)
{
	def main(args: Array[String])
	{
		parseFile(args(0), (pn:List[PN])=>{ println(PN.output(pn))} )
	}

	def parseFile(filename: String, f: (List[PN]=>Unit))
	{
		for (line <- Source.fromFile(filename).getLines())
		  parseLine(line, f)
	}

	def parseLine(line: String, f: (List[PN]=>Unit))
	{
		line.split(' ') match
		{
			case Array(halflead, pns, halfleadpns) => genTrivialVariations(PN.parseToList(pns), f)
			case _ => Nil.iterator
		}
	}

}
