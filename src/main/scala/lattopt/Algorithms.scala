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

  /**
   * Given a feasible object <code>obj</code>, find a maximal feasible
   * object above <code>obj</code>.
   */
  def maximize[A <: OptLattice[_, _]]
              (lattice : A)
              (obj : lattice.LatticeObject)
              (implicit randomData : RandomDataSource)
             : lattice.LatticeObject = {
    assert(lattice isFeasible obj)

    var current    = obj
    var upperBound = lattice.top
    var stepSize   = 0.5

    while (current != upperBound) {
      val next = lattice.intermediate(current, upperBound, stepSize)

      val diffNext = next != current
      if (diffNext && (lattice isFeasible next)) {
        current = next
        if (stepSize < 0.5)
          stepSize = stepSize * 2.0
      } else {
        if (diffNext) {
          for (newBound <- lattice.oneStepDifference(current, next))
            upperBound = lattice.meet(upperBound, newBound)
          stepSize = stepSize / 2.0
        }

        // try to go just one step then
        val it = lattice succ current
        
        var found = false
        while (!found && it.hasNext) {
          val next = it.next
          if (lattice.latticeOrder.lteq(next, upperBound)) {
            if (lattice isFeasible next) {
              current = next
              found = true
            } else {
              for (newBound <- lattice.oneStepDifference(current, next))
                upperBound = lattice.meet(upperBound, newBound)
            }
          }
        }

        if (!found)
          upperBound = current
      }
    }

    current
  }

}
