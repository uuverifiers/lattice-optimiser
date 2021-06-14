
package lattopt;

class ProductLattice[LabelA, LabelB, CostA, CostB,
                     A <: OptLattice[LabelA, CostA],
                     B <: OptLattice[LabelB, CostB]]
                    (val a : A, val b : B)
                    extends OptLattice[(LabelA, LabelB), (CostA, CostB)] {
  type LatticeObject = (a.LatticeObject, b.LatticeObject)

  override def toString =
    "" + a + " * " + b

  def nodeCount : BigInt = a.nodeCount * b.nodeCount

  val top = (a.top, b.top)
  val bottom = (a.bottom, b.bottom)

  // normal partial order
  val latticeOrder = new PartialOrdering[LatticeObject] {
    def tryCompare(x: LatticeObject, y: LatticeObject) =
      for (c1 <- a.latticeOrder.tryCompare(x._1, y._1);
           c2 <- b.latticeOrder.tryCompare(x._2, y._2);
           if (c1 * c2 >= 0))
      yield (c1, c2) match {
        case (x, _) if (x > 0) => 1
        case (_, x) if (x > 0) => 1
        case (y, _) if (y < 0) => -1
        case (_, y) if (y < 0) => -1
        case _ => 0
      }
 
    def lteq(x: LatticeObject, y: LatticeObject) =
      a.latticeOrder.lteq(x._1, y._1) && b.latticeOrder.lteq(x._2, y._2)
  }

  def meet(x: LatticeObject, y: LatticeObject): LatticeObject =
      (a.meet(x._1, y._1), b.meet(x._2, y._2))

  def join(x: LatticeObject, y: LatticeObject): LatticeObject =
      (a.join(x._1, y._1), b.join(x._2, y._2))
      
  def oneStepDifference(lower : LatticeObject,
                        upper : LatticeObject) : Option[LatticeObject] =
    if (lower._1 == upper._1)
      for (x <- b.oneStepDifference(lower._2, upper._2))
      yield (a.top, x)
    else if (lower._2 == upper._2)
      for (x <- a.oneStepDifference(lower._1, upper._1))
      yield (x, b.top)
    else
      None

  // normal order
  def succ(x: LatticeObject): Iterator[LatticeObject] =
    (for (as <- a.succ(x._1)) yield (as, x._2)) ++ (
     for (bs <- b.succ(x._2)) yield (x._1, bs))

  def pred(x: LatticeObject): Iterator[LatticeObject] =
    (for (ap <- a.pred(x._1)) yield (ap, x._2)) ++ (
     for (bp <- b.pred(x._2)) yield (x._1, bp))
     
  def isFeasible(x: LatticeObject) =       
    a.isFeasible(x._1) && b.isFeasible(x._2)
 
  def toCost(x : LatticeObject) =
    (a.toCost(x._1), b.toCost(x._2))
    
  def getLabel(x : LatticeObject) =
    (a.getLabel(x._1), b.getLabel(x._2)) 

  ///////////////////////////////////////////////////////////////////////////////////

  /**
     For c = comp:

     o is comparable to c
     <=>
     !(o <= c) && !(o >= c)
     <=>
     !(o1 <= c1 && o2 <= c2) && !(o1 >= c1 && o2 >= c2)
     <=>
     (!(o1 <= c1) || !(o2 <= c2)) && (!(o1 >= c1) || !(o2 >= c2))

     <=>
     !(o1 <= c1) && !(o1 >= c1) ||
     !(o2 <= c2) && !(o2 >= c2) ||
     !(o1 <= c1) && !(o2 >= c2) ||
     !(o1 >= c1) && !(o2 <= c2)

     <=>
     o1 and c1 are incomparable ||
     o2 and c2 are incomparable ||
     !(o1 <= c1) && !(o2 >= c2) ||
     !(o1 >= c1) && !(o2 <= c2)

     <=>
     o1 and c1 are incomparable ||
     o2 and c2 are incomparable ||
     (o1 > c1) && (o2 < c2) ||
     (o1 < c1) && (o2 > c2)

   */
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

      val incompLeft  = a.incomparableFeasibleObjects(lowerBound1, comp1)
      val incompRight = b.incomparableFeasibleObjects(lowerBound2, comp2)
  
      /* (o1 < c1) && (o2 > c2) */
      val allIncompRight =
        if (a.latticeOrder.lt(lowerBound1, comp1)) {
          val rightSuccessors =
            b.join(lowerBound2, comp2) match {
              case `comp2` => b succ comp2
              case x       => Iterator single x
            }

          Util.minObjects(incompRight ++ (rightSuccessors filter b.isFeasible))(
                          b.latticeOrder)
        } else {
          incompRight
        }
  
      /* (o1 > c1) && (o2 < c2) */
      val allIncompLeft =
        if (b.latticeOrder.lt(lowerBound2, comp2)) {
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
  
      (for (x <- allIncompRight) yield (lowerBound1, x)) ++
      (for (x <- allIncompLeft)  yield (x, lowerBound2))

    } else {
      Iterator single lowerBound
    }

  sanityCheck

}


