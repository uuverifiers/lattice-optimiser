package lattopt;

import org.scalacheck.Properties

object IntervalsTests extends Properties("Intervals") {
  property("Simple Interval([2,4])") = {

    val lattice1 = IntervalLattice(2,4)

    val x = lattice1.succ(lattice1.bottom).next
    lattice1.getLabel(x) == (Some(2),Some(2))

    val xs = ((for (o <- lattice1.succ(lattice1.bottom);
                   o2 <- lattice1.succ(o))
    yield o2).toList)

    xs == List((None,Some(2)), (Some(2),Some(3)), (Some(2),Some(3)), (Some(3),Some(4)), (Some(3),Some(4)), (Some(4),None))
  }
}