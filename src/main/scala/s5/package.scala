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
    val input = scala.io.Source.fromFile("src/main/scala/s5/tsp2.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    val cities = my_input.map(x => {
      val a = x.split(" ")(0).toDouble
      val b = x.split(" ")(1).toDouble
      new City(a, b)
    }).toArray

    println(cities.size)
    var map = HashMap[(List[Int], Int), Double]()
    var c_arr = Array.ofDim[Double](n + 1, n + 1)
    map.update((List(1), 1), 0.0)
    for (s <- 2 to n) {
      val subsets: List[List[Int]] = (2 to n).toSet.subsets(s).map(_.toList.::(1)).toList
      println(subsets.size)
      for (sub <- subsets) {
        map.update((sub.sorted, 1), 999999999.999)
        for (j <- sub) {
          map.update((sub.sorted, j),
            sub.foldLeft(77777777.7777)((acc, i) => {
              if (i != j) {
                val c: Double = map.get((sub.filter(_ != j), i)).getOrElse(0)
                val d = distance(cities(i - 1), cities(j - 1))
                println(d)
                val res = c + d
                if (res < acc) res else acc
              }
              else acc
            }))
        }
      }
    }
    println(map)
    (2 to n).foreach(x => println(map.get(((1 to n).toList, x)).getOrElse(888.88) + distance(cities(x - 1), cities(0))))


  }
}
