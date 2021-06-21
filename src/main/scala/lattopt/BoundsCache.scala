package lattopt;

import scala.collection.mutable.ArrayBuffer

object PredicateCache {
  def apply[A <: Lattice[_]]
           (_lattice : A)
           (pred : _lattice.LatticeObject => Boolean) =
    new PredicateCache[A, _lattice.LatticeObject](_lattice) {
      def predicate(obj : _lattice.LatticeObject) = pred(obj)
    }
}

/**
 * A class for caching the results of a monotonically increasing
 * predicate on some lattice. The class minimises the number of calls
 * to the <code>predicate</code> method by storing bounds on the
 * objects for which the predicate is true/false.
 * 
 * TODO: optimise the class using decision trees.
 */
abstract class PredicateCache[A <: Lattice[_], LObject]
                             (protected val lattice : A) {

  import lattice.{LatticeObject, latticeOrder}

  def predicate(obj : LObject) : Boolean

  private val trueBounds, falseBounds = new ArrayBuffer[LatticeObject]

  def apply(_obj : LObject) : Boolean = {
    val obj = _obj.asInstanceOf[LatticeObject]
    if (trueBounds exists (latticeOrder.lteq(_, obj))) {
      true
    } else if (falseBounds exists (latticeOrder.lteq(obj, _))) {
      false
    } else {
      predicate(_obj) match {
        case false => {
          falseBounds += obj
          false
        }
        case true => {
          trueBounds += obj
          true
        }
      }
    }
  }

}
