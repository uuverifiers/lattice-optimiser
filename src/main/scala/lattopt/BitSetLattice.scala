
package lattopt;

import scala.collection.immutable.BitSet

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

  val top    = BitSet(0 until width : _*)
  val bottom = BitSet()

  def join(x: LatticeObject, y: LatticeObject): LatticeObject = x | y
  def meet(x: LatticeObject, y: LatticeObject): LatticeObject = x & y

  def succ (obj: LatticeObject) : Iterator[LatticeObject] =
    for (t <- (top -- obj).iterator) yield (obj + t)

  def pred(obj: LatticeObject) : Iterator[LatticeObject] =
    for (t <- obj.iterator) yield (obj - t)

  def getLabel(x : LatticeObject) : BitSet = x

  def toCost(x : LatticeObject) : Int = x.size

  def isFeasible(x : LatticeObject) : Boolean = true

  def nodeCount : BigInt = BigInt(1) << width

  def oneStepDifference(lower : LatticeObject,
                        upper : LatticeObject) : Option[LatticeObject] = {
    val step = upper -- lower
    if (step.size == 1) Some(top ^ step) else None
  }

  override def toString : String = "BitSetLattice(" + width + ")"

  sanityCheck

}
