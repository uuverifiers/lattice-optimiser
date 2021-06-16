
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
    !lattice1.isFeasible(lattice1.top) && {
      val lb = (lattice1 succ lattice1.bottom).next
      (lattice1 succ lb) forall {
        comp =>
        val incomps = lattice1.incomparableFeasibleObjects(lb, comp).toList
        (incomps forall {
           x =>
           lattice1.latticeOrder.lteq(lb, x) &&
           lattice1.latticeOrder.tryCompare(comp, x).isEmpty &&
           lattice1.isFeasible(x) &&
           (incomps forall {
              y => x == y || lattice1.latticeOrder.tryCompare(x, y).isEmpty })
         }) && (
          lattice1.feasibleObjectIterator forall {
            x =>
            !lattice1.latticeOrder.lteq(lb, x) ||
            !lattice1.latticeOrder.tryCompare(comp, x).isEmpty ||
            (incomps exists { y => lattice1.latticeOrder.lteq(y, x) })
          }
        )
      }
    }
  }

}
