
package lattopt;

abstract class DelegatingOptLattice[Label, Score, A <: OptLattice[_, _]]
                                   (val underlying : A)
         extends OptLattice[Label, Score] {

  type LatticeObject = underlying.LatticeObject

  val top : LatticeObject = underlying.top
  val bottom : LatticeObject = underlying.bottom

  val latticeOrder = underlying.latticeOrder

  def meet(x: LatticeObject, y: LatticeObject): LatticeObject =
    underlying.meet(x, y)

  def join(x: LatticeObject, y: LatticeObject): LatticeObject =
    underlying.join(x, y)

  def succ(x: LatticeObject): Iterator[LatticeObject] =
    underlying.succ(x)

  def pred(x: LatticeObject): Iterator[LatticeObject] =
    underlying.pred(x)

  def intermediate(lower : LatticeObject,
                   upper : LatticeObject,
                   position : Double)
                  (implicit randomData : RandomDataSource)
                : LatticeObject =
    underlying.intermediate(lower, upper, position)

  def nodeCount : BigInt = underlying.nodeCount

  def oneStepDifference(feasible : LatticeObject,
                        infeasible : LatticeObject) : Option[LatticeObject] =
    underlying.oneStepDifference(feasible, infeasible)

  def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                  comp : LatticeObject)
                                 : Iterator[LatticeObject] =
    underlying.incomparableFeasibleObjects(lowerBound, comp)

  def objectIterator : Iterator[LatticeObject] =
    underlying.objectIterator

  def feasibleObjectIterator : Iterator[LatticeObject] =
    underlying.feasibleObjectIterator

}

////////////////////////////////////////////////////////////////////////////////

class SameTypeDelegatingOptLattice[Label, Score, A <: OptLattice[Label, Score]]
                                  (underlying1 : A)
      extends DelegatingOptLattice[Label, Score, A](underlying1) {

  val scoreOrder = underlying.scoreOrder

  def getLabel(x : LatticeObject) : Label =
    underlying.getLabel(x)

  def toScore(x : LatticeObject) : Score =
    underlying.toScore(x)

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

}

////////////////////////////////////////////////////////////////////////////////

object RelabeledLattice {

  def apply[Label, Label1, Score]
           (underlying : OptLattice[Label, Score], mapping : Label => Label1)
          : OptLattice[Label1, Score] = underlying match {
    case underlying : RelabeledLattice[_, Label, Score, _] =>
      new RelabeledLattice(underlying.underlying,
                           underlying.mapping andThen mapping)
    case _ =>
      new RelabeledLattice(underlying, mapping)
  }

}

class RelabeledLattice[Label, Label1, Score, A <: OptLattice[Label, Score]] private
                      (underlying1 : A, val mapping : Label => Label1)
      extends DelegatingOptLattice[Label1, Score, A](underlying1) {

  val scoreOrder = underlying.scoreOrder

  def getLabel(x : LatticeObject) : Label1 =
    mapping(underlying.getLabel(x))

  def toScore(x : LatticeObject) : Score =
    underlying.toScore(x)

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

  override def toString : String =
    "Relabeled(" + underlying.toString + ")"

  sanityCheck

}

////////////////////////////////////////////////////////////////////////////////

/*
object ReScoredLattice {

  def apply[Label, Score, Score1]
           (underlying : OptLattice[Label, Score], mapping : Label => Score1,
            scoreOrder : Ordering[Score1])
          : OptLattice[Label, Score1] = underlying match {
    case underlying : ReScoreedLattice[Label, _, Score, OptLattice[Label, Score]] =>
      new ReScoreedLattice[Label, Score, Score1, OptLattice[Label, Score]](
                          underlying.underlying, mapping, scoreOrder)
    case _ =>
      new ReScoreedLattice[Label, Score, Score1, OptLattice[Label, Score]](
                          underlying, mapping, scoreOrder)
  }

}
 */

class ReScoredLattice[Label, Score, Score1, A <: OptLattice[Label, Score]]
                     (underlying1 : A,
                      val mapping : Label => Score1,
                      val scoreOrder : Ordering[Score1])
      extends DelegatingOptLattice[Label, Score1, A](underlying1) {

  def getLabel(x : LatticeObject) : Label =
    underlying.getLabel(x)

  def toScore(x : LatticeObject) : Score1 =
    mapping(underlying.getLabel(x))

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

  override def toString : String =
    "ReScoreed(" + underlying.toString + ")"

  sanityCheck

}

////////////////////////////////////////////////////////////////////////////////

object FilteredLattice {
  def apply[Label, Score]
           (underlying : OptLattice[Label, Score], pred : Label => Boolean)
          : OptLattice[Label, Score] = underlying match {
    case underlying : FilteredLattice[Label, Score, _] =>
      new FilteredLattice(underlying.underlying, x => underlying.pred(x) && pred(x))
    case _ =>
      new FilteredLattice(underlying, pred)
  }
}

class FilteredLattice[Label, Score, A <: OptLattice[Label, Score]] private
                     (underlying1 : A, val pred : Label => Boolean)
     extends SameTypeDelegatingOptLattice[Label, Score, A](underlying1) {

  private def evalPred(obj : LatticeObject) = pred(getLabel(obj))

  override def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x) && evalPred(x)

  override def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                           comp : LatticeObject)
                                         : Iterator[LatticeObject] =
    underlying.incomparableFeasibleObjects(lowerBound, comp) filter evalPred

  override def toString : String =
    "Filtered(" + underlying.toString + ")"

  override def feasibleObjectIterator : Iterator[LatticeObject] =
    underlying.feasibleObjectIterator filter evalPred

  sanityCheck

}

////////////////////////////////////////////////////////////////////////////////

abstract class ObjectFilteredLattice[Label, Score, A <: OptLattice[Label, Score]]
                                    (underlying1 : A)
         extends SameTypeDelegatingOptLattice[Label, Score, A](underlying1) {

  def filteringPred(x : LatticeObject) : Boolean

  override def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x) && filteringPred(x)

  override def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                           comp : LatticeObject)
                                         : Iterator[LatticeObject] =
    underlying.incomparableFeasibleObjects(lowerBound, comp) filter filteringPred

  override def toString : String =
    "Filtered(" + underlying.toString + ")"

  override def feasibleObjectIterator : Iterator[LatticeObject] =
    underlying.feasibleObjectIterator filter filteringPred

  sanityCheck

}

////////////////////////////////////////////////////////////////////////////////

object CachedFilteredLattice {
  def apply[Label, Score]
           (underlying : OptLattice[Label, Score], pred : Label => Boolean)
          : OptLattice[Label, Score] =
    new CachedFilteredLattice(underlying, pred)
}

class CachedFilteredLattice[Label, Score, A <: OptLattice[Label, Score]] private
                           (underlying1 : A, val pred : Label => Boolean)
     extends SameTypeDelegatingOptLattice[Label, Score, A](underlying1) {

  private val predCache =
    new PredicateCache[A, underlying.LatticeObject](underlying) {
      def predicate(obj : underlying.LatticeObject) : Boolean =
        !pred(underlying.getLabel(obj))
    }

  private def evalPred(obj : LatticeObject) = !predCache(obj)

  override def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x) && evalPred(x)

  override def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                           comp : LatticeObject)
                                         : Iterator[LatticeObject] =
    underlying.incomparableFeasibleObjects(lowerBound, comp) filter evalPred

  override def toString : String =
    "CachedFiltered(" + underlying.toString + ")"

  override def feasibleObjectIterator : Iterator[LatticeObject] =
    underlying.feasibleObjectIterator filter evalPred

  sanityCheck

}

////////////////////////////////////////////////////////////////////////////////

/**
 * Inverted lattice in which <code>top</code> and <code>bottom</code>
 * are swappepd.
 */
class InvertedLattice[Label, A <: Lattice[Label]] (val underlying : A)
      extends Lattice[Label] {
  type LatticeObject = underlying.LatticeObject

  override val latticeOrder = new PartialOrdering[LatticeObject] {
    def tryCompare(x: LatticeObject, y: LatticeObject) =
      underlying.latticeOrder.tryCompare(y, x)
    def lteq(x: LatticeObject, y: LatticeObject) =
      underlying.latticeOrder.lteq(y, x)
  }

  override val top    = underlying.bottom
  override val bottom = underlying.top

  override def join(x: LatticeObject, y: LatticeObject): LatticeObject =
    underlying.meet(x, y)

  override def meet(x: LatticeObject, y: LatticeObject): LatticeObject =
    underlying.join(x, y)

  /** Compute the direct parents/successors of an object */
  override def succ(x: LatticeObject): Iterator[LatticeObject] =
    underlying.pred(x)

  /** Compute the direct children/predecessors of an object */
  override def pred(x: LatticeObject): Iterator[LatticeObject] =
    underlying.succ(x)

  def intermediate(lower : LatticeObject,
                   upper : LatticeObject,
                   position : Double)
                  (implicit randomData : RandomDataSource)
                : LatticeObject =
    underlying.intermediate(upper, lower, 1.0 - position)

  def nodeCount : BigInt = underlying.nodeCount

  def getLabel(x : LatticeObject) : Label =
    underlying.getLabel(x)

  def objectIterator : Iterator[LatticeObject] =
    underlying.objectIterator

}
