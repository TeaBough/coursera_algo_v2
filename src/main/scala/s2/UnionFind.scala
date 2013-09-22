package s2

import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
/**
 * Union Find implementaion.
 * Find is O(1)
 * Union is O(log(n))
 * Implementation is using a HashTable. Each wrap has a set which maintains the elements in that wrap.
 * When 2 wraps are union, then both the set's are clubbed. O(log(n)) operation
 * A HashMap is also maintained to find the Wrap associated with each node. O(log(n)) operation in mainitaining it.
 *
 * If the input array is null at any index, it is ignored
 */
class UnionFind[T](all: Array[T]) {
  private var dataStruc = new HashMap[T, Wrap]
  for (a <- all
       if(a!=null)
  )
    dataStruc = dataStruc + (a -> new Wrap(a))


  var timeU = 0L
  var timeF = 0L
  /**
   * THe number of Unions
   */
  private var size = dataStruc.size

  /**
   * Unions the set containing a and b
   */
  def union(a: T, b: T): Wrap = {
    val st = System.currentTimeMillis()
    val first: Wrap = dataStruc.get(a).get
    val second: Wrap = dataStruc.get(b).get
    if (first.contains(b) || second.contains(a))
      first
    else {
      //below is to merge smaller with bigger rather than other way around
      val firstIsBig = (first.set.size > second.set.size)
      val ans = if (firstIsBig) {
        first.set = first.set ++ second.set
        second.set.foreach(a => {
          dataStruc = dataStruc - a
          dataStruc = dataStruc + (a -> first)
        })
        first
      } else {
        second.set = second.set ++ first.set
        first.set.foreach(a => {
          dataStruc = dataStruc - a
          dataStruc = dataStruc + (a -> second)
        })
        second
      }
      timeU = timeU + (System.currentTimeMillis() - st)
      size = size - 1
      ans

    }

  }

  /**
   * true if they are in same set. false if not
   */
  def find(a: T, b: T): Boolean = {
    val st = System.currentTimeMillis()
    val ans = dataStruc.get(a).get.contains(b)
    timeF = timeF + (System.currentTimeMillis() - st)
    ans
  }

  def sizeUnion: Int = size

  class Wrap(e: T) {
    var set = new HashSet[T]
    set = set + e

    def add(elem: T) {
      set = set + elem
    }

    def contains(elem: T): Boolean = set.contains(elem)

  }

}
