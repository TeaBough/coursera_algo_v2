package s6

import scala.util.control.Breaks._
import scala.collection.mutable.HashMap

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 18/10/13
 * Time: 14:08
 */

/*
TODO : Refactor the backtracking method using a HashMap instead of a list
Using a list is too SLOW ! Using more than 100 clauses might take an eternity !
See the preProcessing method
 */
object TwoSAT {
  
  type Clause = List[Int]
  type Formula = List[Clause]

  def replaceWithTrue(c: Clause, a: Int): Option[Clause] = {
    val x = c.head
    val y = c.tail.head
    if (x == a || y == a)
      None
    else if (x == a * (-1) && y != a * (-1)) Option(List(0, y))
    else if (y == a * (-1) && x != a * (-1)) Option(List(x, 0))
    else if (y == a * (-1) && x == a * (-1)) Option(List(0, 0))
    else Option(List(x, y))
  }

  def replaceWithFalse(c: Clause, a: Int): Option[Clause] = {
    val left = c.head
    val right = c.tail.head
    if (left == a * (-1) || right == a * (-1))
      None
    else if (left == a && right != a) Option(List(0, right))
    else if (right == a && left != a) Option(List(left, 0))
    else if (right == a && left == a) Option(List(0, 0))
    else Option(List(left, right))
  }

  def choose(P: List[Formula]): (Formula, List[Formula]) = {
    val sortedP = P sortBy (_.size)
    (sortedP.head, sortedP.tail)
  }

  def expand(f: Formula): List[Formula] = {
    val value = f.head
    val left = value.head
    val right = value.tail.head
    val x = if (left == 0) right else left

    val subT = f.map(c => replaceWithTrue(c, x)).filter(!_.isEmpty).map(_.getOrElse(List(0, 0)))
    val subF = f.map(c => replaceWithFalse(c, x)).filter(!_.isEmpty).map(_.getOrElse(List(0, 0)))
    List(subF, subT)
  }

  def test(f: Formula): String = {
    if (f.isEmpty)
      "SUCCESS"
    else if (f.head.head == 0 && f.head.tail.head == 0)
      "FAIL"
    else {
      "PRROOOUUUTTT"
    }
  }

  trait Doer {
    def doit
  }


  def Backtracking(_S: Formula): String = {
    if (_S.size == 0) "SUCCESS"
    else {
      val nothingDoer = new Doer {
        def doit {}
      }
      var S: List[Formula] = List(_S)
      breakable {
        while (!S.isEmpty) {
          val res = choose(S)
          val p = res._1
          S = res._2
          val subP: List[Formula] = expand(p)
          subP.foreach(pb => {
            val result: String = test(pb)
            if (result == "SUCCESS") break
            else if (result == "FAIL") nothingDoer
            else S = (S.::(pb))
          })

        }
      }
      if (S.isEmpty) "EPIC FAIL" else "SUCCESS"
    }
  }

  def preProcessing(formular: HashMap[Int, Set[List[Int]]], size: Int): HashMap[Int, Set[List[Int]]] = {

    def preProcessingR: Unit = {
      val variables = formular.keys
      variables.foreach(x => {
        if (formular.get(x * (-1)).isEmpty) {
          val nei: Set[List[Int]] = formular.get(x).getOrElse(Set())
          nei.flatten.foreach(xx => {
            val cl = formular.get(xx).getOrElse(Set())
            formular += (xx -> cl.filterNot(p => p.contains(x)))
            if (formular.get(xx).getOrElse(Set()).isEmpty) formular.remove(xx)
          })
        }
      })
    }
    preProcessingR
    var res1 = 0
    var res2 = 1
    while ((res1 != res2) && (res2 != 0)) {
      res1 = formular.size
      preProcessingR
      preProcessingR
      res2 = formular.size
    }
    formular
  }

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s6/2sat3.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    val hhmap = HashMap[Int, Set[List[Int]]]()
    val P = my_input.map(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      List(a, b)
    })
    my_input.foreach(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      hhmap += (a -> hhmap.get(a).getOrElse(Set()).+(List(a, b)))
      hhmap += (b -> hhmap.get(b).getOrElse(Set()).+(List(a, b)))
    })

    println("Start Preprocessing")
    val PP = preProcessing(hhmap, n)
    println("Finished Preprocessing")
    println(Backtracking(PP.values.flatten.toSet.toList))
  }
}
