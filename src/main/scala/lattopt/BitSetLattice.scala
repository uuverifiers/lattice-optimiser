
package lattopt;

import scala.collection.immutable.BitSet
import scala.collection.mutable.{BitSet => MBitSet}

object BitSetLattice {

  def apply(width : Int) : OptLattice[BitSet, Int] =
    new BitSetLattice(width)

  def inverted(width : Int) : OptLattice[BitSet, Int] = {
    val lat = new BitSetLattice(width)
    for (w <- lat) yield (w ^ lat.top)
  }

}

object PowerSetLattice {
  def apply[A](elements : Seq[A]) : OptLattice[Set[A], Int] = {
    val indexed = elements.toIndexedSeq
    for (bs <- BitSetLattice(elements.size)) yield {
      (for (ind <- bs.iterator) yield indexed(ind)).toSet
    }
  }

  def inverted[A](elements : Seq[A]) : OptLattice[Set[A], Int] = {
    val indexed = elements.toIndexedSeq
    for (bs <- BitSetLattice.inverted(elements.size)) yield {
      (for (ind <- bs.iterator) yield indexed(ind)).toSet
    }
  }
}

class BitSetLattice private (width : Int) extends OptLattice[BitSet, Int] {
  type LatticeObject = BitSet

  val latticeOrder  = new PartialOrdering[BitSet] {
    def tryCompare(x: BitSet, y: BitSet) =
      (x subsetOf y, y subsetOf x) match {
        case (true,  true)  => Some(0)
        case (false, true)  => Some(1)
        case (true,  false) => Some(-1)
        case (false, false) => None
      }
    def lteq(x: BitSet, y: BitSet) =
      x subsetOf y
  }

  val costOrder = Ordering.Int

  val top    = BitSet(0 until width : _*)
  val bottom = BitSet()

  def join(x: LatticeObject, y: LatticeObject): LatticeObject = x | y
  def meet(x: LatticeObject, y: LatticeObject): LatticeObject = x & y

  def succ (obj: LatticeObject) : Iterator[LatticeObject] =
    for (t <- (top -- obj).iterator) yield (obj + t)

  def pred(obj: LatticeObject) : Iterator[LatticeObject] =
    for (t <- obj.iterator) yield (obj - t)

  def intermediate(lower : LatticeObject,
                   upper : LatticeObject,
                   position : Double)
                  (implicit randomData : RandomDataSource)
                : LatticeObject = {
    val res = new MBitSet
    res ++= lower

    for (ind <- upper &~ lower)
      if (randomData.nextDouble <= position)
        res += ind

    res.toImmutable
  }

  def getLabel(x : LatticeObject) : BitSet = x

  def toCost(x : LatticeObject) : Int = x.size

  def isFeasible(x : LatticeObject) : Boolean = true

  def nodeCount : BigInt = BigInt(1) << width

  def oneStepDifference(lower : LatticeObject,
                        upper : LatticeObject) : Option[LatticeObject] = {
    val step = upper -- lower
    if (step.size == 1) Some(top ^ step) else None
  }

  def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                  comp : LatticeObject)
                                 : Iterator[LatticeObject] =
    if (latticeOrder.lteq(comp, lowerBound))
      Iterator.empty
    else if (latticeOrder.lteq(lowerBound, comp))
      for (t <- (top ^ comp).iterator) yield (lowerBound + t)
    else
      Iterator single lowerBound

  override def toString : String = "BitSetLattice(" + width + ")"

  def objectIterator : Iterator[LatticeObject] = new Iterator[BitSet] {
    private val cur = new MBitSet
    def hasNext = !cur(width)
    def next = {
      val r = cur.toImmutable
      var n = 0
      while (cur(n)) {
        cur -= n
        n = n + 1
      }
      cur += n
      r
    }
  }

  def feasibleObjectIterator = objectIterator

  sanityCheck

}
