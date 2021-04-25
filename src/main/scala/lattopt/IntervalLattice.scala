package lattopt

object IntervalLattice {
  def apply(lowerLimit : BigInt, upperLimit : BigInt) =
    new IntervalLattice(lowerLimit, upperLimit)
}

class IntervalLattice protected (lowerLimit : BigInt, upperLimit : BigInt)
  extends Lattice[(BigInt,BigInt)] {

  type LatticeObject = (Option[BigInt], Option[BigInt])

  val bottom: (Option[scala.math.BigInt], Option[scala.math.BigInt]) = ???
  def getLabel(x: (Option[scala.math.BigInt], Option[scala.math.BigInt])): (scala.math.BigInt, scala.math.BigInt) = ???
  def join(x: (Option[scala.math.BigInt], Option[scala.math.BigInt]),y: (Option[scala.math.BigInt], Option[scala.math.BigInt])): (Option[scala.math.BigInt], Option[scala.math.BigInt]) = ???
  val latticeOrder: scala.math.PartialOrdering[(Option[scala.math.BigInt], Option[scala.math.BigInt])] = ???
  def meet(x: (Option[scala.math.BigInt], Option[scala.math.BigInt]),y: (Option[scala.math.BigInt], Option[scala.math.BigInt])): (Option[scala.math.BigInt], Option[scala.math.BigInt]) = ???
  def nodeCount: BigInt = ???
  def pred(x: (Option[scala.math.BigInt], Option[scala.math.BigInt])): Iterator[(Option[scala.math.BigInt], Option[scala.math.BigInt])] = ???
  def succ(x: (Option[scala.math.BigInt], Option[scala.math.BigInt])): Iterator[(Option[scala.math.BigInt], Option[scala.math.BigInt])] = ???
  val top: (Option[scala.math.BigInt], Option[scala.math.BigInt]) = ???

}