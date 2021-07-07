
package lattopt;

import org.scalacheck.Properties

object PowerSetLatticeTests extends Properties("PowerSetLattice") {

  property("Simple PowerSetLattice") = {
    object ABC extends Enumeration {
      val A, B, C = Value
    }

    val abcLattice = PowerSetLattice(ABC.values.toSeq)

    abcLattice.nodeCount == 8 &&
    (abcLattice.getLabel(abcLattice.top) contains ABC.C)
  }

  property("PowerSetLattice with scores") = {
    object ABC extends Enumeration {
      val A, B, C = Value
    }

    val abcLattice =
      PowerSetLattice withScores List((ABC.A, 10), (ABC.B, -5), (ABC.C, 1))

    abcLattice.toScore(abcLattice.bottom) == 0 &&
    abcLattice.toScore(abcLattice.top) == 6
  }

}
