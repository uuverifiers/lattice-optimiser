
package lattopt;

import org.scalacheck.Properties

object DependentProductLatticeTests extends Properties("DependentProductLattice") {

  property("Simple DependentProductLattice") = {
    val lattice1 =
      for (bv1 <- BitSetLattice(4); bv2 <- BitSetLattice(6);
           if !bv1(0) || !bv2(0))
      yield (bv1, bv2, bv1.size + bv2.size)

    lattice1.getLabel(lattice1.top)._3 == 10 &&
    lattice1.isFeasible(lattice1.bottom) &&
    !lattice1.isFeasible(lattice1.top)
  }

}
