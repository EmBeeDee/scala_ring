package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.Row

import scala.xml.Elem

/**
 * Wraps a {@link FullExtension} with its method name (if found in the libraries), and a score (not currently used).
 *
 * @author MBD
 */
case class EvaluatedExtension(extension: FullExtension, libraryName: Option[String], score: Int)
{
	 val nameAndStage: Option[String] = libraryName match {
		 case None => None
		 case Some(name) => Some(name+" "+Row.Stages(extension.method.nbells-1))
	 }
	 override def toString = extension.toString()

	 def toXML(includeType: Boolean): Elem = extension.toXML(includeType, libraryName)
 }