package s6
import scala.util.control.Breaks._

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 18/10/13
 * Time: 14:08
 * To change this template use File | Settings | File Templates.
 */
object TwoSAT {
/*  def TransformTwoSatInputToGraph(l: List[(String,String)]): List[String] = {
    l.flatMap(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      List((a * (-1)).toString() + " " + b.toString(), (b * (-1)).toString() + " " + a.toString() )
    })
  }*/
  type Clause = (String,String)
  type Formula = List[Clause]

  def replaceWithTrue(c: Clause, x: String): Clause = {
    if (x.contains("-")) {
    val left = if (c._1 == x) "T" else if (c._1 == x.substring(1)) "F" else c._1
    val right = if (c._2 == x) "T" else if (c._2 == x.substring(1)) "F" else c._2
      (left,right)
    }
    else {
      val left = if (c._1 == x) "T" else if (c._1 == "-" + x) "F" else c._1
      val right = if (c._2 == x) "T" else if (c._2 == "-" + x) "F" else c._2
      (left,right)
    }
  }

  def replaceWithFalse(c: Clause, x:String): Clause = {
    if (x.contains("-")) {
      val left = if (c._1 == x) "F" else if (c._1 == x.substring(1)) "T" else c._1
      val right = if (c._2 == x) "F" else if (c._2 == x.substring(1)) "T" else c._2
      (left,right)
    }
    else {
    val left = if (c._1 == x) "F" else if (c._1 == "-" + x) "T" else c._1
    val right = if (c._2 == x) "F" else if (c._2 == "-" + x) "T" else c._2
    (left,right)
    }
  }

  def choose(P : List[Formula]): (Formula,List[Formula]) = {
    val sortedP = P sortBy(_.size)
    (sortedP.head,sortedP.tail)
  }

  def expand(f: Formula): List[Formula] = {
    val value = f.head
    val x = if (value._1 == "F") value._2 else value._1

    val subT = f.map(c =>replaceWithTrue(c,x)).filterNot( c => (c._1 == "T") || (c._2 == "T"))
    val subF = f.map(c =>replaceWithFalse(c,x)).filterNot( c => (c._1 == "T") || (c._2 == "T"))
    List(subF,subT)
  }

  def test(f: Formula): String = {
    if (f.isEmpty)
      "SUCCESS"
    else if (f.head._1 == "F" && f.head._2 == "F")
      "FAIL"
    else if (f.head._1 == "T" || f.head._2 == "T")
       test(f.tail)
    else {
      "PRROOOUUUTTT"
    }
  }
       trait Doer {
  def doit
}


  def Backtracking(_S: Formula): String  = {
    val nothingDoer = new Doer {
      def doit {}
    }
    var S: List[Formula] = List(_S)
    breakable {
      while (!S.isEmpty) {
        val size = S.size
        println(size)
        val res = choose(S)
        val  p = res._1
        S = res._2
        val subP:List[Formula] = expand(p)
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

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s6/test.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    val P = my_input.map(line => {
      val a = line.split(" ")(0)
      val b = line.split(" ")(1)
      (a,b)
    })
    println(Backtracking(P))


  }


}
