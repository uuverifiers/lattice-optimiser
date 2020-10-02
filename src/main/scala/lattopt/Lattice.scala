package lattopt;

/**
 * The parent of all lattices. Each lattice has a
 * <code>LatticeObject</code> type, the internal type representing
 * objects of the lattice; and a <code>Label</code> type defining the
 * labels of lattice objects.
 */
trait Lattice[Label] {
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
  def getLabel(x : LatticeObject) : Label

  /** Check whether the lattice is well-defined **/
  protected def sanityCheck {
    assert(latticeOrder.gteq(top, bottom))
  }

  /** Number of nodes of this lattice */
  def nodeCount : BigInt
}

/**
 * The parent of all optimisation lattices. Each optimisation lattice
 * is a lattice, and has in addition a <code>Cost</code> type defining
 * the range of the objective function.
 */
trait OptLattice[Label, Cost] extends Lattice[Label] {

  /** Map a lattice object to its cost */
  def toCost(x : LatticeObject) : Cost

  /** Check whether the given lattice object is feasible */
  def isFeasible(x : LatticeObject) : Boolean
  
  /** Check whether the lattice is well-defined **/
  protected override def sanityCheck {
    super.sanityCheck
    assert(isFeasible(bottom))
  }

  /** Assuming that <code>infeasible > feasible</code>,
      return an object <code>result</code> such that
      <code>feasible = meet(infeasible, result)</code>,
      and such that, whenever <code>feasible <= x</code>, and <code>x</code>
      is feasible, it holds that <code>x <= result</code>. */
  def feasibilityBound(feasible : LatticeObject,
                       infeasible : LatticeObject) : LatticeObject

  //////////////////////////////////////////////////////////////////////////////

  /** Map labels to some new type, updating the <code>getLabel</code>
      method */
  def map[Label1](mapping : Label => Label1) : OptLattice[Label1, Cost] =
    RelabeledLattice(this, mapping)

  /** Filter out objects of the lattice by updating the
      <code>isFeasible</code> method */
  def filter(pred : Label => Boolean) : OptLattice[Label, Cost] =
    FilteredLattice(this, pred)

}


