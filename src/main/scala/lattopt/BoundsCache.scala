package lattopt;

import scala.collection.mutable.ArrayBuffer

object PredicateCache {
  def apply[Label, A <: Lattice[Label]]
           (_lattice : A)
           (pred : _lattice.LatticeObject => Boolean) =
    new PredicateCache[Label, A](_lattice) {
      def predicate(obj : lattice.LatticeObject) =
        pred(obj.asInstanceOf[_lattice.LatticeObject])
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
abstract class PredicateCache[Label, A <: Lattice[Label]]
                             (protected val lattice : A) {

  import lattice.{LatticeObject, latticeOrder}

  def predicate(obj : LatticeObject) : Boolean

  private val trueBounds, falseBounds = new ArrayBuffer[LatticeObject]

  def apply(obj : LatticeObject) : Boolean =
    if (trueBounds exists (latticeOrder.lteq(_, obj))) {
      true
    } else if (falseBounds exists (latticeOrder.lteq(obj, _))) {
      false
    } else {
      predicate(obj) match {
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
