package lattopt;

import org.scalacheck.Properties

object AlgorithmsTests extends Properties("Algorithms") {

    val lattice1 =
      for (bv1 <- BitSetLattice(4); bv2 <- BitSetLattice(6);
           if !bv1(0) || !bv2(0))
      yield (bv1, bv2, bv1.size + bv2.size)

  property("incomparableFeasibleObjects1") = {

    val it1 = lattice1 succ lattice1.bottom
    it1.next
    it1.next

    val lb = it1.next

    val it2 = lattice1 succ lb
    it2.next
    val comp1 = it2.next
    it2.next
    val comp2 = it2.next
    val comps = List(comp1, comp2)

    val incomps =
      Algorithms.incomparableFeasibleObjects(lattice1)(lb, comps).toList

    correctIncomps(lattice1)(lb, comps, incomps)

  }

  property("incomparableFeasibleObjects2") = {

    val it1 = lattice1 succ lattice1.bottom
    it1.next
    it1.next

    val lb = it1.next

    val comp1 = (lattice1 succ lb).next
    val comp2 = (lattice1 succ comp1).next
    val comps = List(comp1, comp2)

    val incomps =
      Algorithms.incomparableFeasibleObjects(lattice1)(lb, comps).toList

    correctIncomps(lattice1)(lb, comps, incomps)

  }

  def correctIncomps(lattice : OptLattice[_, _])
                    (lb : lattice.LatticeObject,
                     comps : Seq[lattice.LatticeObject],
                     incomps : Seq[lattice.LatticeObject]) = {

        (incomps forall {
           x =>
           lattice.latticeOrder.lteq(lb, x) &&
           (comps forall {
              y => lattice.latticeOrder.tryCompare(x, y).isEmpty }) &&
           lattice.isFeasible(x)
         }) && (
          lattice.feasibleObjectIterator forall {
            x =>
            !lattice.latticeOrder.lteq(lb, x) ||
            !(comps forall {
                y => lattice.latticeOrder.tryCompare(x, y).isEmpty }) ||
            (incomps exists { y => lattice.latticeOrder.lteq(y, x) })
          }
        )

  }

}
