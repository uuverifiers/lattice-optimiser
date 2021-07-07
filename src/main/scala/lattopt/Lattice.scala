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

  /**
   * Randomly pick an element between <code>lower</code> and
   * <code>upper</code>. The <code>position</code> value determines
   * how far the element can be expected to be from <code>lower</code>
   * and <code>upper</code>; for <code>position == 0.0</code>, the
   * returned element will be equal to <code>lower</code>, for
   * <code>position == 1.0</code> it will be equal to
   * <code>upper</code>.
   */
  def intermediate(lower : LatticeObject,
                   upper : LatticeObject,
                   position : Double)
                  (implicit randomData : RandomDataSource)
                : LatticeObject

  /** Map a lattice object to the corresponding foreground object */
  def getLabel(x : LatticeObject) : Label

  /** Number of nodes of this lattice */
  def nodeCount : BigInt

  /** Iterate over the objects (represented via their labels) of this
    * lattice */
  def iterator : Iterator[Label] =
    objectIterator map getLabel

  /** Iterate over the objects of this lattice */
  def objectIterator : Iterator[LatticeObject]

  /** Check whether the lattice is well-defined **/
  protected def sanityCheck {
    assert(latticeOrder.gteq(top, bottom))
  }
}

/**
 * The parent of all optimisation lattices. Each optimisation lattice
 * is a lattice, and has in addition a <code>Score</code> type defining
 * the range of the objective function. The <code>isFeasible</code>
 * function defines which of the objects of the lattice are considered
 * during search and optimisation.
 */
trait OptLattice[Label, Score] extends Lattice[Label] {

  /** A total order on scores, which is used for optimization */
  val scoreOrder : Ordering[Score]

  /** Map a lattice object to its score */
  def toScore(x : LatticeObject) : Score

  /** Check whether the given lattice object is feasible */
  def isFeasible(x : LatticeObject) : Boolean
  
  /** Check whether the lattice is well-defined **/
  protected override def sanityCheck {
    super.sanityCheck
    assert(isFeasible(bottom))
    assert(scoreOrder.lteq(toScore(bottom), toScore(top)))
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
  def map[Label1](mapping : Label => Label1) : OptLattice[Label1, Score] =
    RelabeledLattice(this, mapping)

  /** Choose the score of objects as a function of the labels */
  def withScore[Score1](mapping : Label => Score1)
                       (implicit scoreOrder : Ordering[Score1])
                      : OptLattice[Label, Score1] =
    new ReScoredLattice[Label, Score, Score1, this.type](this, mapping,
                                                         scoreOrder)

  /** Filter out objects of the lattice by updating the
      <code>isFeasible</code> method */
  def filter(pred : Label => Boolean) : OptLattice[Label, Score] =
    FilteredLattice(this, pred)

  /** Filter out objects of the lattice by updating the
      <code>isFeasible</code> method */
  def filterObjects(p : LatticeObject => Boolean) : OptLattice[Label, Score] =
    new ObjectFilteredLattice[Label, Score, this.type](this) {
      def filteringPred(x : LatticeObject) : Boolean = p(x)
    }

  /** Filter out objects of the lattice by updating the
      <code>isFeasible</code> method */
  def cachedFilter(pred : Label => Boolean) : OptLattice[Label, Score] =
    CachedFilteredLattice(this, pred)

  /** Construct a dependent product */
  def flatMap[Label1, Score1]
             (mapping : Label => OptLattice[Label1, Score1])
            : OptLattice[Label1, (Score, Score1)] =
    new DependentProductLattice(this, mapping)

  def *[Label1, Score1, That <: OptLattice[Label1, Score1]] (that : That)
       : OptLattice[(Label, Label1), (Score, Score1)] =
    new ProductLattice(this, that)

}


