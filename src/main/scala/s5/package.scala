/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 11/10/13
 * Time: 14:49
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.mutable.HashMap
import scala.collection.SortedSet

object s5 {

  case class City(left: Double, right: Double)


  def NGosperHack(s: Int, k: Int, n: Int): Int = {
    //var set:Int = (1 << k) - 1
    var set: Int = s
    val limit: Int = (1 << n);
    if (set < limit) {

      // Gosper's hack:
      val c: Int = set & -set;
      val r: Int = set + c;
      (((r ^ set) >>> 2) / c) | r;
    }
    else -1
  }

  def distance(c1: City, c2: City): Double = {
    math.sqrt((c1.left - c2.left) * (c1.left - c2.left) + (c1.right - c2.right) * (c1.right - c2.right))
  }

  def arrayToString(a: Array[Array[Double]]): String = {
    val str = for (l <- a) yield l.mkString("{", "   |   ", "}")
    str.mkString("", ",\n", "")
  }

  def getIndexFromBin(seq: Int): List[Int] = {
    (1 to 25).filter(x => seq / math.pow(2, x - 1).toInt % 2 == 1).toList
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

    // println(NGosperHack(1,1,10))

    //val max_size = (1 to 25).toSet.subsets(12).size
    var new_map = HashMap[(Int, Int), Double]()
    var old_map = HashMap[(Int, Int), Double]()
    //var c_arr = Array.ofDim[Double](math.pow(2,n).toInt,2)
    old_map.update((1, 1), 0.0)
   // println((30.0 / math.pow(2, 1)).toInt)
  //  println("AHHH " + ((1 << 8) - 1))
   val t0 = System.nanoTime()
    for (s <- 2 to n) {
      println(s)
      old_map = new_map
      old_map.update((1, 1), 0.0)
      new_map = HashMap[(Int, Int), Double]()
      var seq: Int = (1 << s) - 1
      while (seq != -1) {
        if (seq % 2 == 1) {
          val l: List[Int] = getIndexFromBin(seq).sorted.filter(_ != 1)
          for (j <- l) {
            val min = l.::(1).foldLeft(2147423647.77)((acc, i) => {
              if (i != j) {
                val c: Double = old_map.get((seq - math.pow(2, j-1).toInt, i)).getOrElse(2147453647.666)
                val d = distance(cities(i - 1), cities(j - 1))
                val res = c + d
                if (res < acc) res else acc
              }
              else acc
            })
            new_map.update((seq, j), min)
          }
        }
        seq = NGosperHack(seq, s, n-1)
      }
    }
    val res = (2 to n).map(x => new_map.get((1 << n) - 1, x).getOrElse(2147003647.88) + distance(cities(x - 1), cities(0)))
    val t1 = System.nanoTime()
    println(new_map)
    println(res.foldLeft(2000483647.9999)((acc, num) => math.min(acc, num)))
    println("Chrono Non : " + (t1 - t0).toDouble / 1000000000 + "s")

  }
}
