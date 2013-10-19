package s6

import scala.util.control.Breaks._
import scala.collection.mutable.HashMap

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 18/10/13
 * Time: 14:08
 * To change this template use File | Settings | File Templates.
 */
object TwoSAT {
  /*  def TransformTwoSatInputToGraph(l: List[(Int,Int)]): List[Int] = {
      l.flatMap(line => {
        val a = line.split(" ")(0).toInt
        val b = line.split(" ")(1).toInt
        List((a * (-1)).toInt() + " " + b.toInt(), (b * (-1)).toInt() + " " + a.toInt() )
      })
    }*/
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
    val x = c.head
    val y = c.tail.head
    if (x == a * (-1) || y == a * (-1))
      None
    else if (x == a && y != a) Option(List(0, y))
    else if (y == a && x != a) Option(List(x, 0))
    else if (y == a && x == a) Option(List(0, 0))
    else Option(List(x, y))
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
          val size = S.size
          //println(size)
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

  def preProcessing(l: Formula, size: Int, map:HashMap[Int, Int]): TwoSAT.Formula = {
    var hmap = map
    def preProcessingR(formular: Formula): TwoSAT.Formula = {
      var ll = formular
      val variables: List[Int] = removeUseLessVariables(size, hmap)
      println(variables)
      var i = 0
      variables.foreach(x => {
        //println(variables.size - i)
        i = i +1
        if (!hmap.contains(x * (-1))) {
          val lll = ll
          ll = ll.filter(!_.contains(x))
          lll.diff(ll).foreach( c => {
            hmap.remove(c.head)
            hmap.remove(c.tail.head)
          })
        }
      })
      println("BOOOMMM")
      ll
    }
    var res2 = l
    var res1 = List(List(1))
    while ((res1.size != res2.size) && (res2.size != 0)) {
      res1 = preProcessingR(res2)
      res2 = preProcessingR(res1)
    }
/*    println("BIG MLI " + mli)
    println(l)

    def preProcessingR(l: Formula): Formula = {
      val lmli = (1 to size).foldLeft(List():List[Int])((acc,num) => {
        if (!hmap.get(num).isEmpty) acc.::(num)
        else if (!hmap.get((-1)*num).isEmpty) acc.::(num*(-1))
        else acc }
      )
      */
 /*     println("Pre MLI :  " + lmli)
      println(hmap.keys)
      val mmli = lmli.foldLeft(List():List[Int])((acc,num) => {
        if (hmap.get(num*(-1)).isEmpty) acc.::(num)
        else acc }
      )
      println("To Delete =  " + mmli)
      val res = l.filterNot( x => {
        val intersect = x.intersect(mmli)
        if (!intersect.isEmpty){
          intersect.foreach(z => {
//          mli = mli.diff(List(z))
            hmap.remove(z)
          })
        }
        !intersect.isEmpty
      })
      println("Res : " + res)
      res
    }
    println("==========================================")
    val r = preProcessingR(l)
    println("==========================================")
    val rr = preProcessingR(r)
    println("==========================================")
    val rrr = preProcessingR(rr)
    println("==========================================")
*/
    /*var res2 = l
    var res1 = List(List(1))
    while ((res1.size != res2.size) && (res2.size != 0)) {
      println("==========================================")
      res1 = preProcessingR(res2)
      println("==========================================")
      res2 = preProcessingR(res1)
      println("==========================================")
    }*/
    res2
  }


  def removeUseLessVariables(size: Int, hmap: HashMap[Int, Int]): List[Int] = {
    val positiv_var = (1 to size).foldLeft(List(): List[Int])((acc, num) => {
      if (!hmap.get(num).isEmpty) acc.::(num)
      else acc
    }
    )
    val negativ_var = ((-1) * size to (-1)).foldLeft(List(): List[Int])((acc, num) => {
      if (!hmap.get(num).isEmpty) acc.::(num)
      else acc
    }
    )
    negativ_var.:::(positiv_var)
  }

  def listUseFullVariables(l: List[List[Int]], n:Int): List[Int] = {
    (1 to n).foldLeft(List():List[Int])((acc,num) => {
      if (l.exists(_.contains(num))) acc.::(num)
      else acc }
    )
  }

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s6/testT.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    var hmap = HashMap[Int, Int]()
    var hhmap = HashMap[Int, Set[List[Int]]]()
    val P = my_input.map(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      List(a, b)
    })
    my_input.foreach(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      hmap += (a -> 1)
      hmap += (b -> 1)
    })
    my_input.foreach(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      hhmap += (a -> hhmap.get(a).getOrElse(Set()).+(List(a,b)))
      hhmap += (b -> hhmap.get(a).getOrElse(Set()).+(List(a,b)))
    })

    //val L = listUseFullVariables(P,n)
    println(hhmap.get(-8))
    println("Start Preprocessing")
    //val PP: Formula = preProcessing(P, n,hmap)
    //println(PP.size)



  }


}
