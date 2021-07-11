package lattopt;

import scala.collection.mutable.ArrayBuffer

object Algorithms {

  var debug = false

  /**
   * Enumerate all maximal feasible objects above
   * <code>lowerBound</code>.
   */
  def maximalFeasibleObjects[A <: OptLattice[_, _]]
                            (lattice : A)
                            (lowerBound : lattice.LatticeObject)
                            (implicit randomData : RandomDataSource)
                          : Iterator[lattice.LatticeObject] = {
    assert(lattice isFeasible lowerBound)

    new Iterator[lattice.LatticeObject] {
      import lattice.{LatticeObject => LObject}

      private val incompSolver = new IncomparablesSolver[A, LObject](lattice)
      private var start : Option[LObject] = null

      private def setStart : Unit =
        if (start == null)
          start = incompSolver findIncomparableObject lowerBound

      def hasNext = {
        setStart
        start.isDefined
      }

      def next = {
        setStart
        val res = maximize(lattice)(start.get)
        start = null
        incompSolver addConstraint res
        res
      }
    }
  }

  /**
   * Compute the meet of all maximal feasible objects above
   * <code>lowerBound</code>.
   */
  def maximalFeasibleObjectMeet[A <: OptLattice[_, _]]
                               (lattice : A)
                               (lowerBound : lattice.LatticeObject)
                               (implicit randomData : RandomDataSource)
                              : lattice.LatticeObject = {
    assert(lattice isFeasible lowerBound)
    import lattice.{LatticeObject => LObject}
    
    val incompSolver = new IncomparablesSolver[A, LObject](lattice)
    var currentMeet  = lattice.top

    var cont = (currentMeet != lowerBound)
    while (cont)
      (incompSolver findIncomparableObject lowerBound) match {
        case Some(start) =>
          maximizeNotAbove(lattice)(start, currentMeet) match {
            case Left(maximal) => {
              currentMeet = lattice.meet(currentMeet, maximal)
              incompSolver addConstraint maximal
              cont = (currentMeet != lowerBound)
            }
            case Right(nonMax) => {
              incompSolver addConstraint nonMax
            }
          }
        case None =>
          cont = false
      }

    currentMeet
  }

  /**
   * Compute the maximal feasible objects above
   * <code>lowerBound</code> with maximum score.
   */
  def optimalFeasibleObjects[Label, Score]
                            (lattice : OptLattice[Label, Score])
                            (lowerBound : lattice.LatticeObject)
                            (implicit randomData : RandomDataSource)
                           : Set[lattice.LatticeObject] = {
    assert(lattice isFeasible lowerBound)

    import lattice.{LatticeObject => LObject, toScore, scoreOrder}
    val optima = new ArrayBuffer[LObject]
    var bestScore : Score = toScore(lowerBound)

    val stopCond : (LObject, LObject) => Option[LObject] = {
      (current : LObject, bound : LObject) =>
      if (scoreOrder.lt(toScore(bound), bestScore))
        Some(bound)
      else
        None
    }

    val incompSolver =
      new IncomparablesSolver[OptLattice[Label, Score], LObject](lattice)

    var start = incompSolver.findIncomparableObject(lowerBound)

    while (start.isDefined) {
      maximizeHelp(lattice)(start.get, stopCond) match {
        case Left(opt) =>
          scoreOrder.compare(toScore(opt), bestScore) match {
            case n if n < 0 =>
              incompSolver addConstraint opt
            case 0 => {
              optima += opt
              incompSolver addConstraint opt
              if (debug)
                Console.err.println("New optimum: " + lattice.getLabel(opt))
            }
            case _ => {
              bestScore = toScore(opt)
              if (debug) {
                Console.err.println("New optimum: " + lattice.getLabel(opt))
                Console.err.println("New best score: " + bestScore)
              }
              optima.clear
              optima += opt
              incompSolver addConstraint opt
            }
          }
        case Right(bound) =>
          incompSolver addConstraint bound
      }

      start = incompSolver.findIncomparableObject(lowerBound)
    }

    optima.toSet
  }

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

    val compsSorted =
      comps.toList sortBy { obj => estimateIncomparableNum(lattice)(obj) }

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
   * Given a feasible element <code>lowerBound</code>, compute a set
   * <code>S</code> of feasible objects <code>&gt;= lowerBound</code>
   * that are <ol> <li> incomparable to all objects in
   * <code>comps</code>, and</li> <li> <code>S</code> has the property
   * that for every feasible object <code>o &gt;= lowerBound</code>
   * and <code>o</code> is incomparable to the objects in
   * <code>comps</code>, there is an element <code>u in S</code> such
   * that <code>u &lt;= o</code>. </li> </ol>.
   */
  def incomparableFeasibleObjectsRand[A <: OptLattice[_, _]]
                                 (lattice : A)
                                 (lowerBound : lattice.LatticeObject,
                                  comps : Iterable[lattice.LatticeObject])
                                 (implicit randomData : RandomDataSource)
                               : Iterator[lattice.LatticeObject] = {
    import lattice.{LatticeObject => LObject,
                    latticeOrder => order,
                    incomparableFeasibleObjects => incompFeasibles}

    val compsSorted =
      comps.toList sortBy { obj => estimateIncomparableNum(lattice)(obj) }

//    val compsSorted = (randomData shuffle comps).toList

    def incompHelp(currentLowerBound : LObject,
                   remComps : List[LObject]) : Iterator[LObject] = {
      remComps match {
        case List() =>
          Iterator single currentLowerBound
        case comp :: otherComps =>
          for (obj1 <- randomData.shuffle(
                         incompFeasibles(currentLowerBound,
                                         comp).toList).iterator;
               obj2 <- incompHelp(obj1, otherComps))
          yield obj2
      }
    }

    // TODO: filter objects based on previously returned objects

    incompHelp(lowerBound, compsSorted)
  }

  /**
   * Estimate the number of incomparable objects for the given lattice
   * object. This is used to sort objects in
   * <code>incomparableFeasibleObjects</code>
   */
  private def estimateIncomparableNum(lattice : Lattice[_])
                                     (obj : lattice.LatticeObject) : Int =
    lattice match {
      case lattice : DelegatingOptLattice[_, _, _] =>
        estimateIncomparableNum(
          lattice.underlying)(
          obj.asInstanceOf[lattice.underlying.LatticeObject])
      case lattice : ProductLattice[_, _, _, _, _, _] => {
        val (x, y) = obj.asInstanceOf[lattice.LatticeObject]
        estimateIncomparableNum(lattice.a)(x) +
        estimateIncomparableNum(lattice.b)(y)
      }
      case lattice : DependentProductLattice[_, _, _, _, _, _] => {
        val (x, y) = obj.asInstanceOf[lattice.LatticeObject]
        estimateIncomparableNum(lattice.a)(x) +
        estimateIncomparableNum(lattice.bBaseLattice)(
                                y.asInstanceOf[lattice.BBaseObject])
      }
      case lattice : BitSetLattice =>
        lattice.width - obj.asInstanceOf[lattice.LatticeObject].size
    }

  /**
   * Given a feasible object <code>obj</code>, find a maximal feasible
   * object above <code>obj</code>. The provided stopping condition
   * takes as argument the current best object found during maximization,
   * and the discovered upper bound on feasible objects, and can be used
   * to stop the optimization process.
   */
  def maximizeHelp
              [A <: OptLattice[_, _], B]
              (lattice : A)
              (obj : lattice.LatticeObject,
               stoppingCond :
                   (lattice.LatticeObject, lattice.LatticeObject) => Option[B])
              (implicit randomData : RandomDataSource)
             : Either[lattice.LatticeObject, B] = {
    assert(lattice isFeasible obj)

    var current    = obj
    var upperBound = lattice.top
    var stepSize   = 0.5

    while (current != upperBound) {
      stoppingCond(current, upperBound) match {
        case Some(v) => return Right(v)
        case None    => // continue
      }

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

    Left(current)
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
    def stopCond(x : lattice.LatticeObject,
                 y : lattice.LatticeObject) : Option[Unit] = None
    val Left(r) = maximizeHelp(lattice)(obj, stopCond _)
    r
  }

  /**
   * Given a feasible object <code>obj</code>, find a maximal feasible
   * object above <code>obj</code> (<code>Left</code>). Terminate as
   * soon as the optimization discovers an element that is above
   * <code>notAbove</code> and return that element (<code>Right</code>).
   */
  def maximizeNotAbove[A <: OptLattice[_, _]]
                      (lattice : A)
                      (obj : lattice.LatticeObject,
                       notAbove : lattice.LatticeObject)
                      (implicit randomData : RandomDataSource)
                     : Either[lattice.LatticeObject, lattice.LatticeObject] = {
    import lattice.{LatticeObject => LObject}

    val stopCond : (LObject, LObject) => Option[LObject] = {
      (current, bound) =>
        if (lattice.latticeOrder.lteq(notAbove, current))
          Some(current)
        else
          None
    }

    maximizeHelp(lattice)(obj, stopCond)
  }

}
