package s1

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 13/09/13
 * Time: 17:56
 * To change this template use File | Settings | File Templates.
 */

case class Job(weight: Int, length: Int)

object scheduling {

  def MyOrdering = new Ordering[Job] {
    def compare(a: Job, b: Job) = {
      val difA = a.weight - a.length
      val difB = b.weight - b.length
      if (difA == difB){
        if (a.weight.compare(b.weight) == -1) println(b + " is bigger than " + a)
        else
          println(a + " is bigger than " + b)
        a.weight.compare(b.weight)
      }
      else
      {
        if (difA.compare(difB) == -1) println(b + " is bigger than " + a)
        else
          println(a + " is bigger than " + b)
        difA.compare(difB)
      }
    }
  }


  def main(args: Array[String]) {

    println("ref = " + 1.compare(2))
    val lines = scala.io.Source.fromFile("src/main/scala/s1/jobs.txt", "utf-8").getLines.mkString
    val pq = new mutable.PriorityQueue[Job]()(MyOrdering) ++ (List(Job(30,90),Job(10,30),Job(20,40),Job(30,50),Job(20,60),Job(10,50)))

    println(pq.dequeue())
    println(pq.dequeue())
    println(pq.dequeue())
    println(pq.dequeue())
    println(pq.dequeue())
    println(pq.dequeue())
  }
}
