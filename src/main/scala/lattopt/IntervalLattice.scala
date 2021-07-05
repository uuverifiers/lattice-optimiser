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

  override def toString =
    "[" + lowerLimit + ", " + upperLimit + "]"

  val bottom: (Option[BigInt], Option[BigInt]) = (Some(BigInt(1)), Some(BigInt(0)))
  val top: (Option[BigInt], Option[BigInt]) = (None,None)

  private def neg(x : Option[BigInt]) : Option[BigInt] =
    for (a <- x) yield -a

  private def min(x : Option[BigInt], y : Option[BigInt]) =
    for (a <- x; b <- y) yield (a min b)

  private def min2(x : Option[BigInt], y : Option[BigInt]) =
    (x, y) match {
      case (None, None)       => None
      case (Some(a), None)    => Some(a)
      case (None, Some(a))    => Some(a)
      case (Some(a), Some(b)) => Some(a min b)
    }

  private def max(x : Option[BigInt], y : Option[BigInt]) =
    for (a <- x; b <- y) yield (a max b)

  private def max2(x : Option[BigInt], y : Option[BigInt]) =
    (x, y) match {
      case (None, None)       => None
      case (Some(a), None)    => Some(a)
      case (None, Some(a))    => Some(a)
      case (Some(a), Some(b)) => Some(a max b)
    }
  private def canonise(o : LatticeObject) : LatticeObject =
    if (isEmpty(o)) bottom else o

  private def incUpper(a : BigInt) : Option[BigInt] =
    if (a >= upperLimit) None else Some(a + 1)

  private def decLower(a : BigInt) : Option[BigInt] =
    if (a <= lowerLimit) None else Some(a - 1)

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

  def join(x: (Option[BigInt], Option[BigInt]),y: (Option[BigInt], Option[BigInt])): (Option[BigInt], Option[BigInt]) =
    if (isEmpty(x))
      y
    else if (isEmpty(y))
      x
    else
      canonise((min(x._1, y._1), max(x._2, y._2)))

  def meet(x: (Option[BigInt], Option[BigInt]),y: (Option[BigInt], Option[BigInt])): (Option[BigInt], Option[BigInt]) =
    if (isEmpty(x) || isEmpty(y))
      bottom
    else
      canonise((max2(x._1, y._1), min2(x._2, y._2)))

  def nodeCount: BigInt =
      1 +                                  // empty interval
      (upperLimit - lowerLimit + 2) +      // upper = inf
      (((upperLimit - lowerLimit + 1) * (upperLimit - lowerLimit + 4)) / 2)

  def bigIterator(start: BigInt, end: BigInt, step: BigInt = 1) =
    Iterator.iterate(start)(_ + step).takeWhile(_ < end)

  def succ(x: (Option[BigInt], Option[BigInt])): Iterator[LatticeObject] =
    x match {
      case x if (isEmpty(x)) => // successor of empty is ... [1,1], [2,2], ...
        for (i <- (bigIterator(lowerLimit, upperLimit + 1)))
          yield (Some(i), Some(i))
      case (None, Some(a)) =>
        Iterator((None, incUpper(a)))
      case (Some(a), None) =>
        Iterator((decLower(a), None))
      case (x@Some(a), y@Some(b)) =>
        Iterator((decLower(a), y), (x, incUpper(b)))
      case _ =>
        Iterator.empty
    }

  def pred(x: (Option[BigInt], Option[BigInt])): Iterator[LatticeObject] =
    x match {
      case x if (isEmpty(x)) =>
        Iterator.empty
      case (None, None) =>
        Iterator((None, Some(upperLimit)), (Some(lowerLimit), None))
      case (x@Some(a), None) =>
        Iterator((x, Some(upperLimit))) ++
          (if (a < upperLimit) Iterator((Some(a + 1), None))
          else Iterator.empty)
      case (None, x@Some(a)) =>
        Iterator((Some(lowerLimit), x)) ++
          (if (a > lowerLimit) Iterator((None, Some(a - 1)))
          else Iterator.empty)
      case (x@Some(a), y@Some(b)) =>
        (if (a < upperLimit) Iterator(canonise((Some(a + 1), y)))
        else Iterator.empty) ++
          (if (b > lowerLimit) Iterator(canonise((x, Some(b - 1))))
          else Iterator.empty)
  }


  def objectIterator : Iterator[LatticeObject] = ???

  def feasibleObjectIterator : Iterator[LatticeObject] = ???

  def intermediate(lower : LatticeObject,
                   upper : LatticeObject,
                   position : Double)
                  (implicit randomData : RandomDataSource)
                : LatticeObject = ???

  sanityCheck

}
