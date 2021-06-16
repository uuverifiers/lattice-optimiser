
package lattopt;

/**
 * Products in which the second component can depend on the first
 * component. This class is used to implement the <code>flatMap</code>
 * method.
 */
class DependentProductLattice[LabelA, LabelB, CostA, CostB,
                              A <: OptLattice[LabelA, CostA],
                              B <: OptLattice[LabelB, CostB]]
                             (val a : A, val bFun : LabelA => B)
      extends OptLattice[LabelB, (CostA, CostB)] {

  /**
   * Depending on the chosen A-object, the B-object might have different
   * feasibility and label functions; but the underlying B-lattice needs to
   * have the same shape regardless of the A-object.
   */
  val bBaseLattice : Lattice[LabelB] = bFun(a.getLabel(a.bottom))
  type BBaseObject = bBaseLattice.LatticeObject

  type LatticeObject = (a.LatticeObject, BBaseObject)

  override def toString =
    "" + a + ".flatMap(x => " + bBaseLattice + ")"

  protected override def sanityCheck {
    super.sanityCheck
    assert(bBaseLattice.bottom == bFun(a.getLabel(a.top)).bottom &&
           bBaseLattice.top    == bFun(a.getLabel(a.top)).top)
  }

  def nodeCount : BigInt = a.nodeCount * bBaseLattice.nodeCount

  val top    = (a.top,    bBaseLattice.top)
  val bottom = (a.bottom, bBaseLattice.bottom)

  // normal partial order
  val latticeOrder = new PartialOrdering[LatticeObject] {
    def tryCompare(x: LatticeObject, y: LatticeObject) =
      for (c1 <- a.latticeOrder.tryCompare(x._1, y._1);
           c2 <- bBaseLattice.latticeOrder.tryCompare(x._2, y._2);
           if (c1 * c2 >= 0))
      yield (c1, c2) match {
        case (x, _) if (x > 0) => 1
        case (_, x) if (x > 0) => 1
        case (y, _) if (y < 0) => -1
        case (_, y) if (y < 0) => -1
        case _ => 0
      }
 
    def lteq(x: LatticeObject, y: LatticeObject) =
      a.latticeOrder.lteq(x._1, y._1) &&
      bBaseLattice.latticeOrder.lteq(x._2, y._2)
  }

  def meet(x: LatticeObject, y: LatticeObject): LatticeObject =
    (a.meet(x._1, y._1), bBaseLattice.meet(x._2, y._2))

  def join(x: LatticeObject, y: LatticeObject): LatticeObject =
    (a.join(x._1, y._1), bBaseLattice.join(x._2, y._2))
      
  // normal order
  def succ(x: LatticeObject): Iterator[LatticeObject] =
    (for (as <- a.succ(x._1)) yield (as, x._2)) ++ (
     for (bs <- bBaseLattice.succ(x._2)) yield (x._1, bs))

  def pred(x: LatticeObject): Iterator[LatticeObject] =
    (for (ap <- a.pred(x._1)) yield (ap, x._2)) ++ (
     for (bp <- bBaseLattice.pred(x._2)) yield (x._1, bp))
     
  def isFeasible(x: LatticeObject) =
    a.isFeasible(x._1) && {
      val b = bFun(a.getLabel(x._1))
      b.isFeasible(x._2.asInstanceOf[b.LatticeObject])
    }
 
  def toCost(x : LatticeObject) = {
    val b = bFun(a.getLabel(x._1))
    (a.toCost(x._1), b.toCost(x._2.asInstanceOf[b.LatticeObject]))
  }
    
  def getLabel(x : LatticeObject) = {
    val b = bFun(a.getLabel(x._1))
    b.getLabel(x._2.asInstanceOf[b.LatticeObject])
  }

  def objectIterator : Iterator[LatticeObject] =
    for (x <- a.objectIterator; y <- bBaseLattice.objectIterator)
    yield (x, y)

  def feasibleObjectIterator : Iterator[LatticeObject] =
    for (x <- a.feasibleObjectIterator;
         y <- bFun(a.getLabel(x)).feasibleObjectIterator)
    yield (x, y.asInstanceOf[BBaseObject])

  def oneStepDifference(lower : LatticeObject,
                        upper : LatticeObject) : Option[LatticeObject] =
    if (lower._1 == upper._1) {
      val b = bFun(a.getLabel(lower._1))
      for (x <- b.oneStepDifference(lower._2.asInstanceOf[b.LatticeObject],
                                    upper._2.asInstanceOf[b.LatticeObject]))
      yield (a.top, x.asInstanceOf[BBaseObject])
    } else if (lower._2 == upper._2) {
      for (x <- a.oneStepDifference(lower._1, upper._1))
      yield (x, bBaseLattice.top)
    } else {
      None
    }

  def incomparableFeasibleObjects(lowerBound : LatticeObject,
                                  comp : LatticeObject)
                                : Iterator[LatticeObject] =
    if (latticeOrder.lteq(comp, lowerBound)) {
      Iterator.empty
    } else if (latticeOrder.lteq(lowerBound, comp)) {

      /*
         Cover elements o such that
         o1 and c1 are incomparable || o2 and c2 are incomparable
       */
  
      val (lowerBound1, lowerBound2) = lowerBound
      val (comp1,       comp2)       = comp

      val b = bFun(a.getLabel(lowerBound1))

      val lowerBound2_b = lowerBound2.asInstanceOf[b.LatticeObject]
      val comp2_b       = comp2.asInstanceOf[b.LatticeObject]

      val incompLeft    = a.incomparableFeasibleObjects(lowerBound1, comp1)
      val incompRight   = b.incomparableFeasibleObjects(lowerBound2_b, comp2_b)

      /* (o1 < c1) && (o2 > c2) */
      val allIncompRight =
        if (a.latticeOrder.lt(lowerBound1, comp1)) {
          val rightSuccessors =
            b.join(lowerBound2_b, comp2_b) match {
              case `comp2_b` => b succ comp2_b
              case x         => Iterator single x
            }

          Util.minObjects(incompRight ++ (rightSuccessors filter b.isFeasible))(
                          b.latticeOrder)
        } else {
          incompRight
        }
  
      /* (o1 > c1) && (o2 < c2) */
      val allIncompLeft =
        if (bBaseLattice.latticeOrder.lt(lowerBound2, comp2)) {
          val leftSuccessors =
            a.join(lowerBound1, comp1) match {
              case `comp1` => a succ comp1
              case x       => Iterator single x
            }

          Util.minObjects(incompLeft ++ (leftSuccessors filter a.isFeasible))(
                          a.latticeOrder)
        } else {
          incompLeft
        }
  
      (for (x <- allIncompRight) yield (lowerBound1,
                                        x.asInstanceOf[BBaseObject])) ++
      (for (x <- allIncompLeft)  yield (x, lowerBound2))

    } else {
      Iterator single lowerBound
    }

  sanityCheck

}


