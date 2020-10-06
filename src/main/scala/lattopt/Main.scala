
package lattopt;

object Main extends App {

  {
  println("Lattice test 1")

  val lattice1 = BitSetLattice(8)
  val lattice2 = for (x <- lattice1) yield (for (n <- x) yield (1 << n)).sum

  println(lattice1)
  println(lattice2)

  println(lattice2.getLabel(lattice2.bottom))
  println(lattice2.getLabel(lattice2.top))

  {
    var x = lattice2.bottom
    while (lattice2.succ(x).hasNext) {
      x = lattice2.succ(x).next
      println(lattice2.getLabel(x))
    }
  }
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

}
