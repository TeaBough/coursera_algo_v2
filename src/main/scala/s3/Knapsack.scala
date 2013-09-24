package s3

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 24/09/13
 * Time: 14:20
 * To change this template use File | Settings | File Templates.
 */

import math.max
import scala.collection.immutable.SortedMap



object Knapsack {

  def nonOptimizedKnapsack(map:SortedMap[Int, (Int, Int)], knapsack_nb:Int):SortedMap[(Int,Int),Int] = {
    map.foldLeft(SortedMap[(Int, Int), Int]()) {
      (acc, num) => {
        acc.++({
          (0 until knapsack_nb + 1).foldLeft(SortedMap[(Int, Int), Int]()) {
            (a, n) => {
              a.+((num._1, n) -> (
                math.max(acc.get((num._1 - 1, n)).getOrElse(0),
                  if (n - num._2._2 >= 0) {
                    acc.get((num._1 - 1, n - num._2._2)) match {
                      case Some(y) => y + num._2._1
                      case None => 0
                    }
                  }
                  else 0)
                ))
            }
          }
        })
      }
    }
  }

  def optimizedKnapsack(map:SortedMap[Int, (Int, Int)], knapsack_nb:Int):Int = {
    var previousA: Array[Int] = Array.fill[Int](knapsack_nb+1)(0)
    val currentA: Array[Int] = Array.fill[Int](knapsack_nb+1)(0)


    map.foreach(p => {
      for(i <- 0 until previousA.length){
        currentA.update(i,math.max(previousA(i),if (i - p._2._2 >=0) previousA(i - p._2._2) + p._2._1 else 0))
      }
      previousA = currentA.clone()

    })
    previousA.last
  }

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s3/knapsack_big.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val knapsack_nb = splited_input.head.split(" ")(0).toInt
    val my_input: List[(String, Int)] = splited_input.toList.drop(1).zip(Stream from 1)
    val final_input = my_input.map(x => (x._2, x._1.split(" ")(0).toInt, x._1.split(" ")(1).toInt))
    val map = final_input.foldLeft(SortedMap[Int, (Int, Int)]()) {
      (acc, num) => acc.+(num._1 ->(num._2, num._3))
    }
    map.++(SortedMap[Int, (Int, Int)](1 ->(1, 2)))
    val t2 = System.nanoTime()
    val ress = optimizedKnapsack(map,knapsack_nb)
    val t3 = System.nanoTime()
    println("Optimised : " + ress)
    println("Chrono Opti : " + (t3 - t2).toDouble / 1000000000 + "s")


    val t0 = System.nanoTime()
   // val res = nonOptimizedKnapsack(map,knapsack_nb)
    val t1 = System.nanoTime()
    //println(res.get((nb, knapsack_nb)))
    println("Chrono Non : " + (t1 - t0).toDouble / 1000000000 + "s")
    /*
        val arr = Array.ofDim[Int](nb+1, knapsack_nb+1)
        res.foreach(x => arr(x._1._1)(x._1._2) = x._2)
        def arrayToString(a: Array[Array[Int]]): String = {
          val str = for (l <- a) yield l.mkString("{", ",", "}")
          str.mkString("{", ",\n", "}")
        }
        println(arrayToString(arr))
        println(arr)
    */
  }
}
