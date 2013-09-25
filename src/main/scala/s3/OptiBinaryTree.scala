package s3

import scala.collection.SortedMap

/**
 * User: thibault
 * Date: 25/09/13
 * Time: 16:59
 */
object OptiBinaryTree {

  def optiBinaryTree(n:Int, p:SortedMap[Int,Double]) : Double = {
    val array = Array.ofDim[Double](n+1, n+1)
    var s = 0
    var i = 0
    for (s <- 0 to n-1) {
      for (i <- 1 to n) {
        if (i+s <= n ){
          array(i)(i+s) = {
            (i to i+s).map(r => {
              val a = if (r+1<=n && i+s <= n )  array(r+1)(i+s)  else 0
              val b = (i to i+s).foldLeft(0.0)((acc, num) => acc + p.get(num).getOrElse(0.0))
              array(i)(r-1) + a + b
            }).min
          }
        }
      }
    }
    array(1)(n)
  }

  //Useful function to print matrix
  def arrayToString(a: Array[Array[Double]]): String = {
    val str = for (l <- a) yield l.mkString("{", "   |   ", "}")
    str.mkString("", ",\n", "")
  }

  def main(args: Array[String]) {
    val p = SortedMap[Int,Double](1-> 0.05,2 -> 0.4, 3 -> 0.08, 4 -> 0.04, 5-> 0.1, 6->0.1, 7-> 0.23)
    val n = 7
    println(optiBinaryTree(n,p))
  }
}