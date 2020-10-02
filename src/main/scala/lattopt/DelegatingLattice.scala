
package lattopt;

abstract class DelegatingLattice[Label, Cost, A <: OptLattice[_, _]]
                                (val underlying : A)
      extends OptLattice[Label, Cost] {

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

class SameTypeDelegatingLattice[Label, Cost,
                                A <: OptLattice[Label, Cost]]
                               (underlying1 : A)
      extends DelegatingLattice[Label, Cost, A](underlying1) {

  def getLabel(x : LatticeObject) : Label =
    underlying.getLabel(x)

  def toCost(x : LatticeObject) : Cost =
    underlying.toCost(x)

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

}

////////////////////////////////////////////////////////////////////////////////

object RelabeledLattice {

  def apply[Label, Label1, Cost]
           (underlying : OptLattice[Label, Cost],
            mapping : Label => Label1)
          : OptLattice[Label1, Cost] = underlying match {
    case underlying : RelabeledLattice[_, Label, Cost, _] =>
      new RelabeledLattice(underlying.underlying,
                           underlying.mapping andThen mapping)
    case _ =>
      new RelabeledLattice(underlying, mapping)
  }

}

class RelabeledLattice[Label, Label1, Cost,
                           A <: OptLattice[Label, Cost]] private
                          (underlying1 : A,
                           val mapping : Label => Label1)
      extends DelegatingLattice[Label1, Cost, A](underlying1) {

  def getLabel(x : LatticeObject) : Label1 =
    mapping(underlying.getLabel(x))

  def toCost(x : LatticeObject) : Cost =
    underlying.toCost(x)

  def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x)

}

////////////////////////////////////////////////////////////////////////////////

object FilteredLattice {
  def apply[Label, Cost]
           (underlying : OptLattice[Label, Cost], pred : Label => Boolean)
          : OptLattice[Label, Cost] = underlying match {
    case underlying : FilteredLattice[Label, Cost, _] =>
      new FilteredLattice(underlying.underlying,
                          x => underlying.pred(x) && pred(x))
    case _ =>
      new FilteredLattice(underlying, pred)
  }
}

class FilteredLattice[Label, Cost, A <: OptLattice[Label, Cost]] private
             (underlying1 : A, val pred : Label => Boolean)
     extends SameTypeDelegatingLattice[Label, Cost, A](underlying1) {

  override def isFeasible(x : LatticeObject) : Boolean =
    underlying.isFeasible(x) && pred(getLabel(x))

}
