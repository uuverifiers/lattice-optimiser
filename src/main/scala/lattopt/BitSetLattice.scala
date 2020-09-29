
package lattopt;

import scala.collection.immutable.BitSet

class BitSetLattice(width : Int) extends OptLattice[BitSet, Int] {
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

  def toForeground(x : LatticeObject) : BitSet = x

  def toCost(x : LatticeObject) : Int = x.size

  def isFeasible(x : LatticeObject) : Boolean = true

  def nodeCount : BigInt = BigInt(1) << width

  def feasibilityBound(feasible : LatticeObject,
                       infeasible : LatticeObject) : LatticeObject = {
    val step = feasible -- infeasible
    if (step.size == 1) step else bottom
  }

  override def toString : String = "BitSetLattice(" + width + ")"

}
