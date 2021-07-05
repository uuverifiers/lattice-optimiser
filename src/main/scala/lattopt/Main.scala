
package lattopt;

import scala.collection.immutable.BitSet

object Main extends App {

  {
    println("Lattice test 1")

    val lattice1 = BitSetLattice(8)
    val lattice2 = for (x <- lattice1) yield (for (n <- x) yield (1 << n)).sum

    println(lattice1)
    println(lattice2)

    println(lattice1.incomparableFeasibleObjects(
      BitSet(1).asInstanceOf[lattice1.LatticeObject],
      // lattice1.bottom,
      BitSet(1, 2).asInstanceOf[lattice1.LatticeObject]).toList)
  }

  println

  {
    println("Lattice test 2")

    val lattice1 =
      for (bv1 <- BitSetLattice(4);
           bv2 <- BitSetLattice(6);
           if !bv1(0) || !bv2(0))
      yield (bv1, bv2, bv1.size + bv2.size)

    println(lattice1)
    println("bottom: " + lattice1.getLabel(lattice1.bottom) + ", " +
      lattice1.isFeasible(lattice1.bottom))
    println("top:    " + lattice1.getLabel(lattice1.top) + ", " +
      lattice1.isFeasible(lattice1.top))
  }

  {
    println("Lattice test 3")

    val lattice3 = IntervalLattice(2,4)
    println(lattice3)
    println("top: " + lattice3.top)
    println("bottom: " + lattice3.bottom)
    println(lattice3.succ(lattice3.bottom).toList)
    println((for (o <- lattice3.succ(lattice3.bottom);
                  o2 <- lattice3.succ(o))
    yield o2).toList)

  }


}
