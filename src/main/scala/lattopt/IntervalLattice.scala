package lattopt

object IntervalLattice {
  def apply(lowerLimit : BigInt, upperLimit : BigInt) =
    new IntervalLattice(lowerLimit, upperLimit)
}

class IntervalLattice protected (lowerLimit : BigInt, upperLimit : BigInt)
  extends Lattice[(Option[BigInt],Option[BigInt])] {

  type LatticeObject = (Option[BigInt], Option[BigInt])

  def isEmpty(x: LatticeObject) : Boolean = x match {
    case (Some(a), Some(b)) => a > b
    case _ => false
  }

  def isUniversal(x: LatticeObject) : Boolean = x match {
    case (None, None) => true
    case _ => false
  }

  val bottom: (Option[BigInt], Option[BigInt]) = (Some(BigInt(1)), Some(BigInt(0)))
  val top: (Option[BigInt], Option[BigInt]) = (None,None)

  private def neg(x : Option[BigInt]) : Option[BigInt] =
    for (a <- x) yield -a

  // normal partial order
  val latticeOrder: PartialOrdering[(Option[BigInt], Option[BigInt])] = new PartialOrdering[LatticeObject] {
    def compareLower(x: Option[BigInt],
                             y: Option[BigInt]): Int = (x, y) match {
      case (None, None) => 0
      case (None, Some(_)) => 1
      case (Some(_), None) => -1
      case (Some(a), Some(b)) => b compare a
    }

    def tryCompare(x: LatticeObject, y: LatticeObject) =
      (isEmpty(x), isEmpty(y)) match {
        case (false, false) => {
          // TODO: infinite intervals
          val (xL, xU) = x
          val (yL, yU) = y

          val lRes = compareLower(xL, yL)
          val uRes = compareLower(neg(xU), neg(yU))

          if (lRes * uRes < 0)
            None
          else
            Some(lRes + uRes)
        }
        case (true, false) => Some(-1)
        case (false, true) => Some(1)
        case (true, true) => Some(0)
      }

    def lteq(x: LatticeObject, y: LatticeObject) =
      tryCompare(x, y) match {
        case Some(r) => r <= 0
        case None => false
      }
  }

  def getLabel(x: (Option[BigInt], Option[BigInt]))  = x
  def join(x: (Option[BigInt], Option[BigInt]),y: (Option[BigInt], Option[BigInt])): (Option[BigInt], Option[BigInt]) = ???
  def meet(x: (Option[BigInt], Option[BigInt]),y: (Option[BigInt], Option[BigInt])): (Option[BigInt], Option[BigInt]) = ???
  def nodeCount: BigInt = ???
  def pred(x: (Option[BigInt], Option[BigInt])): Iterator[(Option[BigInt], Option[BigInt])] = ???
  def succ(x: (Option[BigInt], Option[BigInt])): Iterator[(Option[BigInt], Option[BigInt])] = ???

  def objectIterator : Iterator[LatticeObject] = ???

  def feasibleObjectIterator : Iterator[LatticeObject] = ???

}
