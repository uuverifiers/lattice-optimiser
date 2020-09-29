
package lattopt;

class ProductLattice[ForegroundObjectA, ForegroundObjectB, CostA, CostB, A <: OptLattice[ForegroundObjectA,CostA], B <: OptLattice[ForegroundObjectB,CostB]] 
                    (val a : A, val b : B)
                    extends OptLattice[(ForegroundObjectA,ForegroundObjectB),(CostA,CostB)] {
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
 
    def lteq(x: LatticeObject, 
             y: LatticeObject) =
      a.latticeOrder.lteq(x._1, y._1) && b.latticeOrder.lteq(x._2, y._2)
  }

  def meet(x: LatticeObject, y: LatticeObject): LatticeObject =
      (a.meet(x._1, y._1), b.meet(x._2, y._2))

  def join(x: LatticeObject, y: LatticeObject): LatticeObject =
      (a.join(x._1, y._1), b.join(x._2, y._2))
      
  def feasibilityBound(feasible : LatticeObject,
                       infeasible : LatticeObject) : LatticeObject =
    if (feasible._1 == infeasible._1)
      (a.bottom, b.feasibilityBound(feasible._2, infeasible._2))
    else if (feasible._2 == infeasible._2)
      (a.feasibilityBound(feasible._1, infeasible._1), b.bottom)
    else
      bottom

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
    (a.toCost(x._1),b.toCost(x._2))
    
  def toForeground(x : LatticeObject) =
    (a.toForeground(x._1),b.toForeground(x._2)) 

}


