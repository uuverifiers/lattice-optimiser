package lattopt;


object Algorithms {

  /**
   * Given a feasible element <code>lowerBound</code>, compute a set
   * <code>S</code> of feasible objects <code>&gt;= lowerBound</code>
   * that are <ol> <li> incomparable to all objects in
   * <code>comps</code>, and</li> <li> <code>S</code> has the property
   * that for every feasible object <code>o &gt;= lowerBound</code>
   * and <code>o</code> is incomparable to the objects in
   * <code>comps</code>, there is an element <code>u in S</code> such
   * that <code>u &lt;= o</code>. </li> </ol>.
   */
  def incomparableFeasibleObjects[A <: OptLattice[_, _]]
                                 (lattice : A)
                                 (lowerBound : lattice.LatticeObject,
                                  comps : Iterable[lattice.LatticeObject])
                               : Iterator[lattice.LatticeObject] = {
    import lattice.{LatticeObject => LObject,
                    latticeOrder => order,
                    incomparableFeasibleObjects => incompFeasibles}

    // TODO: sort comps in a good way
    val compsSorted = comps.toList

    def incompHelp(currentLowerBound : LObject,
                   remComps : List[LObject]) : Iterator[LObject] = {
      remComps match {
        case List() =>
          Iterator single currentLowerBound
        case comp :: otherComps =>
          for (obj1 <- incompFeasibles(currentLowerBound, comp);
               obj2 <- incompHelp(obj1, otherComps))
          yield obj2
      }
    }

    // TODO: filter objects based on previously returned objects

    incompHelp(lowerBound, compsSorted)
  }

}
