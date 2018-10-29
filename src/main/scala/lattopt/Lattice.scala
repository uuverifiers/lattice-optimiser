

package lattopt;

abstract class OptLattice[ForegroundObject, Cost] {
  type LatticeObject

  val latticeOrder : PartialOrdering[LatticeObject]
  val top, bottom : LatticeObject

  def join(x: LatticeObject, y: LatticeObject): LatticeObject
  def meet(x: LatticeObject, y: LatticeObject): LatticeObject

  /** Compute the direct parents/successors of an object */
  def succ(x: LatticeObject): Iterator[LatticeObject]
  /** Compute the direct children/predecessors of an object */
  def pred(x: LatticeObject): Iterator[LatticeObject]

  /** Map a lattice object to the corresponding foreground object */
  def toForeground(x : LatticeObject) : ForegroundObject

  /** Map a lattice object to its cost */
  def toCost(x : LatticeObject) : Cost

  /** Check whether the given lattice object is feasible */
  def isFeasible(x : LatticeObject) : Boolean

  /** Number of nodes of this lattice */
  def nodeCount : BigInt

  /** Assuming that <code>infeasible > feasible</code>,
      return an object <code>result</code> such that
      <code>feasible = meet(infeasible, result)</code>,
      and such that, whenever <code>feasible <= x</code>, and <code>x</code>
      feasible, it holds that <code>x <= result</code>. */
  def feasibilityBound(feasible : LatticeObject,
                       infeasible : LatticeObject) : LatticeObject

  //////////////////////////////////////////////////////////////////////////////

  /** Map foreground objects to some new type, updating the
      <code>toForeground</code> method */
  def map[ForegroundObject1]
         (mapping : ForegroundObject => ForegroundObject1)
        : OptLattice[ForegroundObject1, Cost] =
    ForegroundMapLattice(this, mapping)

  /** Filter out objects of the lattice by updating the
      <code>isFeasible</code> method */
  def filter(pred : ForegroundObject => Boolean)
           : OptLattice[ForegroundObject, Cost] =
    FilteredLattice(this, pred)

}


