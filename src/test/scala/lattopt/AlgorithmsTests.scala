package lattopt;

import org.scalacheck.Properties

object AlgorithmsTests extends Properties("Algorithms") {

  val lattice1 =
    for (bv1 <- BitSetLattice(4); bv2 <- BitSetLattice(6);
         if !bv1(0) || !bv2(0))
    yield (bv1, bv2, bv1.size + bv2.size)

  val lattice2 =
    for (bv1 <- BitSetLattice(16); bv2 <- BitSetLattice(32);
         if !bv1(0) || !bv2(0) || !bv2(5))
    yield (bv1, bv2, bv1.size + bv2.size)

  val lattice3 =
    for (bv1 <- BitSetLattice(20);
         if ((0 until 10) forall { ind => !bv1(2*ind) || !bv1(2*ind + 1) }))
    yield bv1

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

  property("maximize1") = {
    (0 until 10) forall { seed =>
      implicit val randomData = new SeededRandomDataSource(seed)

      val obj = (lattice2 succ lattice2.bottom).next
      val maxObj = Algorithms.maximize(lattice2)(obj)
      val (bv1, bv2, size) = lattice2 getLabel maxObj

      (!bv1(0) || !bv2(0) || !bv2(5)) && size == 47
    }
  }

  property("optimize1") = {
    (0 until 3) forall { seed =>
      implicit val randomData = new SeededRandomDataSource(seed)

      val result =
        Algorithms.maximalFeasibleObjects(lattice2)(lattice2.bottom).toList

      result.size == 3 &&
      (result reduceLeft lattice2.join) == lattice2.top
    }
  }

  property("maxMeet1") = {
    (0 until 3) forall { seed =>
      implicit val randomData = new SeededRandomDataSource(seed)

      val result =
        Algorithms.maximalFeasibleObjectMeet(lattice2)(lattice2.bottom)
      val (bv1, bv2, size) = lattice2 getLabel result

      (!bv1(0) && !bv2(0) && !bv2(5)) && size == 45
    }
  }

  property("maxMeet2") = {
    implicit val randomData = new SeededRandomDataSource(123)

    val result =
      Algorithms.maximalFeasibleObjectMeet(lattice3)(lattice3.bottom)
    val bv1 = lattice3 getLabel result

    bv1.isEmpty
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
