package net.snowtiger.methodmaker.extension

import scala.xml.Elem

/**
 * Stats accumulated about an extension series, all the series of a parent method, or all parent methods in a library:
 * <ol>
 *   <li>
 * </ol>
 *
 * @author MBD
 */
class ExtensionStats
{
	 var noIndefiniteExtensions = 0
	 var hasIndefiniteExtensions = 0
	 var badExistingExtensions = 0
	 var higherStageExtensionsAllUsed = 0
	 var goodExistingExtensions = 0
	 var unrungUniqueExtensions = 0

	 def print()
	 {
		 println("Methods with no indefinite extensions: "+noIndefiniteExtensions)
		 println("Methods with indefinite extensions: "+hasIndefiniteExtensions)
		 println("Existing extensions which we did not generate: "+badExistingExtensions)
		 println("Methods where all indefinite extensions have been given other names: "+higherStageExtensionsAllUsed)
		 println("Methods whose indefinite extensions have been rung: "+goodExistingExtensions)
		 println("Methods with unique unrung indefinite extensions: "+unrungUniqueExtensions)
	 }

	 def toXML(): Elem =
	 {
		 <Stats>
			 <NoExtensions>{noIndefiniteExtensions}</NoExtensions>
			 <HaveExtensions>{hasIndefiniteExtensions}</HaveExtensions>
			 <BadExtensions>{badExistingExtensions}</BadExtensions>
			 <GoodExtensions>{goodExistingExtensions}</GoodExtensions>
			 <ExtensionsAllUsed>{higherStageExtensionsAllUsed}</ExtensionsAllUsed>
			 <UniqueUnrungExtensions>{unrungUniqueExtensions}</UniqueUnrungExtensions>
		 </Stats>
	 }
 }