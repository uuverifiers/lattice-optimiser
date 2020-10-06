
package lattopt;

import org.scalacheck.Properties

object BitSetLatticeTests extends Properties("BitSetLattice") {

  property("Simple BitSetLattice(8)") = {
    val lattice1 = BitSetLattice(8)
    val lattice2 = for (x <- lattice1) yield (for (n <- x) yield (1 << n)).sum

    var x = lattice2.bottom

    assert(lattice2.getLabel(x) == 0)

    while (lattice2.succ(x).hasNext) {
      x = lattice2.succ(x).next
      assert((0 until 256) contains lattice2.getLabel(x))
    }

    assert(x == lattice2.top && lattice2.getLabel(x) == 255)

    while (lattice2.pred(x).hasNext)
      x = lattice2.pred(x).next

    x == lattice2.bottom
  }

  property("Inverted BitSetLattice(8)") = {
    val lattice = BitSetLattice.inverted(8)

    var x = lattice.bottom
    while (lattice.succ(x).hasNext)
      x = lattice.succ(x).next

    lattice.getLabel(x) == lattice.bottom
  }

}
