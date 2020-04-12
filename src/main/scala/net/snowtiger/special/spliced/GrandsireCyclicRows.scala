package net.snowtiger.special.spliced

import net.snowtiger.ringing.{NamedMethod, Row}

/**
 * @author mark
 */

object GrandsireCyclicRows extends CyclicRowGenerator(NamedMethod("Grandsire", 12, "3T.1T-1T-1T-1T-1T-1T-1T-1T-1T-1T-1T-1T"))
{
	val grandsireCourses1 = List(
		("1234567890ET", 5), ("13547698E02T", 4),
		("14567890ET32", 5), ("157698E03T42", 4),
		("167890ET3254", 5), ("1798E03T5264", 4),
		("1890ET325476", 5), ("19E03T527486", 4),
		("10ET32547698", 5), ("1E3T25749608", 4),
		("1T32547698E0", 5))

	val grandsireCourses2 = List(
		("127593EeT608", 3), ("13547698E02T", 4),
		("14567890ET32", 5), ("157698E03T42", 4),
		("167890ET3254", 5), ("1798E03T5264", 4),
		("1890ET325476", 5), ("19E03T527486", 4),
		("10ET32547698", 5), ("1E3T25749608", 4),
		("1T32547698E0", 5))
	val allRows1 = genRows(grandsireCourses1)
	val allRows2 = genRows(grandsireCourses2)

	// As used for series "Grandsire Cyclic Max 3"
	val grandsireCoursesNew = List(
		("1234567890ET", 5), ("13547698E02T", 5),
		("14567890ET23", 5), ("157698E02T43", 5),
		("167890ET2345", 5), ("1798E02T4365", 5),
		("1890ET234567", 5), ("19E02T436587", 5),
		("10ET23456789", 5), ("1E2T43658709", 5),
		("1T234567890E", 5))
	val allRowsNew = genRows(grandsireCoursesNew)

	// As used for series "Grandsire Cyclic Max 4"
	val grandsireBob = NamedMethod("Bobbed Grandsire", 12, "3T.1T-1T-1T-1T-1T-1T-1T-1T-1T-1T-1T.3T.1T")
	val grandsireCoursesRoundHand1 = List(
		("1T0E89674523", 5), ("1E098765432T", 5),
		("1089674523ET", 5), ("198765432TE0", 5),
		("18674523ET90", 5), ("1765432TE098", 5),
		("164523ET9078", 5), ("15432TE09876", 5),
		("1423ET907856", 5), ("132TE0987654", 5),
		("12ET90785634", 4), ("1357294E6T80", 1))
	val grandsireCoursesRoundHand2 = List(("123547698E0T", 5))
	val allRowsHand = genRows(grandsireCoursesRoundHand1) ++ genRows(grandsireCoursesRoundHand2, grandsireBob) --
			Set(Row("1234567890ET"), Row("132547698E0T"))
}