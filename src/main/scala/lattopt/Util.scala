
package lattopt;

import scala.collection.mutable.ArrayBuffer

object Util {

  def minObjects[A](objs : Iterator[A])
                   (implicit order : PartialOrdering[A]) : Iterator[A] = {
    val seen = new ArrayBuffer[A]
    for (x <- objs; if (!(seen exists { order.lteq(_, x) }))) yield {
      seen += x
      x
    }
  }

}
