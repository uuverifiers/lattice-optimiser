
package lattopt;

import scala.collection.mutable.ArrayBuffer

/**
 * Class to compute incomparable feasible objects in a lattice.
 */
class IncomparablesSolver[A <: OptLattice[_, _], LatticeObject]
                         (val lattice : A) {

  import lattice.{LatticeObject => LObject,
                  latticeOrder => order,
                  incomparableFeasibleObjects => incompFeasibles}

  private var N       = 0
  private val comps   = new ArrayBuffer[LObject]
  private val btCount = new ArrayBuffer[Int]

  /**
   * Add a constraint; subsequent calls to
   * <code>findIncomparableobject</code> will only return objects
   * that are incomparable to all objects added using this method.
   */
  def addConstraint(comp : LatticeObject) : Unit = {
    comps.insert(0, comp.asInstanceOf[LObject])
    btCount.insert(0, 0)
    N = comps.size
  }

  /**
   * Given a feasible element <code>lowerBound</code>, compute some
   * feasible object <code>s &gt;= lowerBound</code>
   * that is <ol> <li> incomparable to all objects in
   * <code>comps</code>. Return <code>None</code> if no such object
   * exists.
   */
  def findIncomparableObject(lowerBound : LatticeObject)
                           : Option[LatticeObject] = {
    assert(lattice isFeasible lowerBound.asInstanceOf[LObject])
    recursionCount = 0
    backtrackCount = 0
    val res = solve(lowerBound.asInstanceOf[LObject], 0)

//    println("#recursions: " + recursionCount)
//    println(btCount.toList)

    res map (_.asInstanceOf[LatticeObject])
  }

  private var recursionCount = 0
  private var backtrackCount = 0

  private def incRecursionCount : Unit = {
    recursionCount = recursionCount + 1
  }

  private def incBacktrackCount : Unit = {
    backtrackCount = backtrackCount + 1
    if (backtrackCount % 100 == 0) {
      var i = 0
      while (i < N) {
        btCount(i) = btCount(i) / 2
        i = i + 1
      }
    }
  }

  /**
   * Recursively search for a feasible incomparable object. This function
   * will try to reorder the <code>comps, btCount</code> buffers dynamically
   * to have the objects that tend to be conflicting in the beginning of the
   * buffers.
   */
  private def solve(currentLowerBound : LObject, n : Int) : Option[LObject] = {
    incRecursionCount
    if (n == N) {
      Some(currentLowerBound)
    } else {
      val it = incompFeasibles(currentLowerBound, comps(n))
      if (it.hasNext) {
        var obj = it.next
        var res = solve(obj, n + 1)
        while (res.isEmpty && it.hasNext) {
          obj = it.next
          res = solve(obj, n + 1)
        }

        if (res.isEmpty) {
//          println("backtracking")

          var maxCount = btCount(n)
          var maxCountIndex = n

          var i = n + 1
          while (i < N) {
            val k = btCount(i)
            if (k > maxCount) {
              maxCount = k
              maxCountIndex = i
            }
            i = i + 1
          }

          if (maxCountIndex > n) {
//            println("reordering")

            val c = comps(maxCountIndex)
            comps remove maxCountIndex
            comps.insert(n, c)

            btCount remove maxCountIndex
            btCount.insert(n, maxCount)
          }
        }

        res
      } else {
        btCount(n) = btCount(n) + 1
        incBacktrackCount
        None
      }
    }
  }

}
