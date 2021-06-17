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

  /** Iterate over the objects (represented via their labels) of this
    * lattice */
  def iterator : Iterator[Label] =
    objectIterator map getLabel

  /** Iterate over the objects of this lattice */
  def objectIterator : Iterator[LatticeObject]
}

/**
 * The parent of all optimisation lattices. Each optimisation lattice
 * is a lattice, and has in addition a <code>Cost</code> type defining
 * the range of the objective function. The <code>isFeasible</code>
 * function defines which of the objects of the lattice are considered
 * during search and optimisation.
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

  /** Iterate over the feasible objects (represented via their labels)
    * of this lattice */
  def feasibleIterator : Iterator[Label] =
    feasibleObjectIterator map getLabel

  /** Iterate over the feasible objects of this lattice */
  def feasibleObjectIterator : Iterator[LatticeObject]

  //////////////////////////////////////////////////////////////////////////////

  /**
   * Assuming that <code>upper > lower</code>, check whether an object
   * <code>result</code> exists such that <code>lower = meet(upper,
   * result)</code>, and such that, whenever <code>lower <= x</code>
   * but not <code>upper <= x</code>, it holds that <code>x <= result</code>.
   */
  def oneStepDifference(lower : LatticeObject,
                        upper : LatticeObject) : Option[LatticeObject]

  /**
   * Given a feasible element <code>lowerBound</code>, compute a set
   * <code>S</code> of feasible objects <code>&gt;= lowerBound</code>
   * that are <ol> <li> incomparable to <code>comp</code>, and</li>
   * <li> <code>S</code> has the property that for every feasible
   * object <code>o &gt;= lowerBound</code> and <code>o</code> is
   * incomparable to <code>comp</code>, there is an element <code>u in
   * S</code> such that <code>u &lt;= o</code>. </li>  </ol>.
   */
  def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                  comp : LatticeObject)
                                 : Iterator[LatticeObject]

  //////////////////////////////////////////////////////////////////////////////

  /** Map labels to some new type, updating the <code>getLabel</code>
      method */
  def map[Label1](mapping : Label => Label1) : OptLattice[Label1, Cost] =
    RelabeledLattice(this, mapping)

  /** Filter out objects of the lattice by updating the
      <code>isFeasible</code> method */
  def filter(pred : Label => Boolean) : OptLattice[Label, Cost] =
    FilteredLattice(this, pred)

  /** Construct a dependent product */
  def flatMap[Label1, Cost1]
             (mapping : Label => OptLattice[Label1, Cost1])
            : OptLattice[Label1, (Cost, Cost1)] =
    new DependentProductLattice(this, mapping)

  def *[Label1, Cost1, That <: OptLattice[Label1, Cost1]] (that : That)
       : OptLattice[(Label, Label1), (Cost, Cost1)] =
    new ProductLattice(this, that)

}


