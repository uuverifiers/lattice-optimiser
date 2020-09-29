
package lattopt;

object Main extends App {

  println("Lattice test")

  val lattice1 = new BitSetLattice(8)
  val lattice2 = for (x <- lattice1) yield (for (n <- x) yield (1 << n)).sum

  println(lattice1)
  println(lattice2)

  println(lattice2.toForeground(lattice2.bottom))
  println(lattice2.toForeground(lattice2.top))

  {
    var x = lattice2.bottom
    while (lattice2.succ(x).hasNext) {
      x = lattice2.succ(x).next
      println(lattice2.toForeground(x))
    }
  }

}
