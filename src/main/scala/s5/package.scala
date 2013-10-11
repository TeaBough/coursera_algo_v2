/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 11/10/13
 * Time: 14:49
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.mutable.HashMap

object s5 {

  case class City(left: Double, right: Double)

  def distance(c1: City, c2: City): Double = {
    math.sqrt((c1.left - c2.left) * (c1.left - c2.left) + (c1.right - c2.right) * (c1.right - c2.right))
  }

  def arrayToString(a: Array[Array[Double]]): String = {
    val str = for (l <- a) yield l.mkString("{", "   |   ", "}")
    str.mkString("", ",\n", "")
  }

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s5/tsp.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    val cities = my_input.map(x => {
      val a = x.split(" ")(0).toDouble
      val b = x.split(" ")(1).toDouble
      new City(a, b)
    }).toArray
    val max_size = (1 to 25).toSet.subsets(12).size)
    var map = HashMap[(List[Int], Int), Double]()
    var c_arr = Array.ofDim[Double](n + 1, n + 1)
    map.update((List(1), 1), 0.0)
    for (s <- 1 to n-1) {
      println(s)
      val subsets: List[List[Int]] = (2 to n).toSet.subsets(s).map(_.toList.::(1)).toList
      for (sub <- subsets) {
        val sorted_sub = sub.sorted
        map.update((sorted_sub, 1), 999999999.999)
        for (j <- sorted_sub.filter(_ != 1)) {
          val min = sorted_sub.foldLeft(77777777.7777)((acc, i) => {
            if (i != j) {
              val c: Double = map.get((sorted_sub.filter(_ != j), i)).getOrElse(66666666.666)
              val d = distance(cities(i - 1), cities(j - 1))
              val res = c + d
              if (res < acc) res else acc
            }
            else acc
          })
          map.update((sorted_sub, j),min)
        }
      }
    }

    val res = (2 to n).map(x => map.get(((1 to n).toList, x)).getOrElse(888.88) + distance(cities(x - 1), cities(0)))
    println(res.foldLeft(9999999999.9999)((acc,num) => math.min(acc,num)))



  }
}
