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
      if (difA == difB)
        a.weight.compare(b.weight)
      else
        difA.compare(difB)
    }
  }


  def main(args: Array[String]) {

    val lines = scala.io.Source.fromFile("src/main/scala/s1/jobs.txt", "utf-8").getLines.mkString
    val pq = new mutable.PriorityQueue[Job]()(MyOrdering) ++ (List(Job(30,90),Job(10,30),Job(20,40),Job(30,50),Job(20,60),Job(10,50)))
    val ll = pq.dequeueAll.toList
    val list_completionTime_weight = ll.foldLeft(List((0,0)))((acc,b) => acc.::((acc.head._1 + b.length,b.weight)))
    val weighted_completionTime = list_completionTime_weight.foldLeft(0)((acc, b) => acc+b._1*b._2)
    println(weighted_completionTime)

  }
}
