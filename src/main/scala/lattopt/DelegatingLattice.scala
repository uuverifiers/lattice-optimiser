
package lattopt;

abstract class DelegatingLattice[ForegroundObject, Cost, A <: OptLattice[_, _]]
                                (val underlying : A)
      extends OptLattice[ForegroundObject, Cost] {

  type LatticeObject = underlying.LatticeObject
//  override def toString = underlying.toString

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

  def nodeCount : BigInt = underlying.nodeCount

  def feasibilityBound(feasible : LatticeObject,
                       infeasible : LatticeObject) : LatticeObject =
    underlying.feasibilityBound(feasible, infeasible)

}

////////////////////////////////////////////////////////////////////////////////

class SameTypeDelegatingLattice[ForegroundObject, Cost,
                                A <: OptLattice[ForegroundObject, Cost]]
                               (underlying1 : A)
      extends DelegatingLattice[ForegroundObject, Cost, A](underlying1) {

  def toForeground(x : LatticeObject) : ForegroundObject =
    underlying.toForeground(x)

  def toCost(x : LatticeObject) : Cost =
    underlying.toCost(x)

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

}

////////////////////////////////////////////////////////////////////////////////

object ForegroundMapLattice {

  def apply[ForegroundObject, ForegroundObject1, Cost]
           (underlying : OptLattice[ForegroundObject, Cost],
            mapping : ForegroundObject => ForegroundObject1)
          : OptLattice[ForegroundObject1, Cost] = underlying match {
    case underlying : ForegroundMapLattice[_, ForegroundObject, Cost, _] =>
      new ForegroundMapLattice(underlying.underlying,
                               underlying.mapping andThen mapping)
    case _ =>
      new ForegroundMapLattice(underlying, mapping)
  }

}

class ForegroundMapLattice[ForegroundObject, ForegroundObject1, Cost,
                           A <: OptLattice[ForegroundObject, Cost]] private
                          (underlying1 : A,
                           val mapping : ForegroundObject => ForegroundObject1)
      extends DelegatingLattice[ForegroundObject1, Cost, A](underlying1) {

  def toForeground(x : LatticeObject) : ForegroundObject1 =
    mapping(underlying.toForeground(x))

  def toCost(x : LatticeObject) : Cost =
    underlying.toCost(x)

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

}

////////////////////////////////////////////////////////////////////////////////

object FilteredLattice {
  def apply[ForegroundObject, Cost]
           (underlying : OptLattice[ForegroundObject, Cost],
            pred : ForegroundObject => Boolean)
          : OptLattice[ForegroundObject, Cost] = underlying match {
    case underlying : FilteredLattice[ForegroundObject, Cost, _] =>
      new FilteredLattice(underlying.underlying,
                          x => underlying.pred(x) && pred(x))
    case _ =>
      new FilteredLattice(underlying, pred)
  }
}

class FilteredLattice[ForegroundObject, Cost,
                      A <: OptLattice[ForegroundObject, Cost]] private
             (underlying1 : A,
              val pred : ForegroundObject => Boolean)
     extends SameTypeDelegatingLattice[ForegroundObject, Cost, A](underlying1) {

  override def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x) && pred(toForeground(x))

}
