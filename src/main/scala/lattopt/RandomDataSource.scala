
package lattopt;

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/**
 * Class to produce data needed to randomise proof construction.
 */
abstract class RandomDataSource {

  /**
   * Tell whether the class actually produces random data. If not,
   * <code>nextBoolean</code> will always return <code>false</code>,
   * <code>nextInt</code> will always return <code>0</code>.
   */
  def isRandom : Boolean

  /**
   * Produce a random Boolean value.
   */
  def nextBoolean : Boolean

  /**
   * Produce a random integer value.
   */
  def nextInt : Int

  /**
   * Produce a random integer value in the range <code>[0, bound)</code>.
   */
  def nextInt(bound : Int) : Int

  /**
   * Produce a random double in the range <code>[0.0, 1.0)</code>.
   */
  def nextDouble : Double

  /**
   * Shuffle the given elements.
   */
  def shuffle[A](seq : Iterable[A]) : Seq[A] = {
    val res = new ArrayBuffer[A]
    res ++= seq

    if (isRandom) {
      val N = res.size
      for (i <- 0 until (N - 1)) {
        val newI = nextInt(N - i) + i
        if (newI != i) {
          val t = res(i)
          res(i) = res(newI)
          res(newI) = t
        }
      }
    }

    res.toSeq
  }

}

/**
 * Source producing non-random data.
 */
object NonRandomDataSource extends RandomDataSource {
  def isRandom : Boolean = false
  def nextBoolean : Boolean = false
  def nextInt : Int = 0
  def nextInt(bound : Int) : Int = 0
  def nextDouble : Double = 0.0
}

/**
 * Source producing random data.
 */
class SeededRandomDataSource(seed : Int) extends RandomDataSource {
  private val rand = new Random (seed)

  def isRandom : Boolean = true
  def nextBoolean : Boolean = rand.nextBoolean
  def nextInt : Int = rand.nextInt
  def nextInt(bound : Int) : Int = rand nextInt bound
  def nextDouble : Double = rand.nextDouble
}
