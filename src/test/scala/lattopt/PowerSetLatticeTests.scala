
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

}
