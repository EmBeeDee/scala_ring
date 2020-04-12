package net.snowtiger.special.spliced

import net.snowtiger.ringing.NamedMethod

/**
 * @author mark
 */

object SlinkyCyclicRows extends CyclicRowGenerator(NamedMethod("Slinky", 12, "-14-14.5T.16.3T.16", "1T"))
{
	val allRows = genRows(cyclicCourses)
}