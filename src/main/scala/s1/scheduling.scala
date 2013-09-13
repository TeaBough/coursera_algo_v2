package s1
import scala.util.matching.Regex
import scala.collection.mutable
import java.text.DecimalFormat

/**
 * Created with DoubleelliJ IDEA.
 * User: thibault
 * Date: 13/09/13
 * Time: 17:56
 * To change this template use File | Settings | File Templates.
 */

case class Job(weight: Double, length: Double)

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

  def MyOrderingRatio = new Ordering[Job] {
    def compare(a: Job, b: Job) = {
      val ratioA = a.weight / a.length
      val ratioB = b.weight / b.length
      if (ratioA == ratioB)
        a.weight.compare(b.weight)
      else
        ratioA.compare(ratioB)
    }
  }

  def ComputeWeightedCompletionTimeRatio(li:List[Job]): Double = {
    val pq =  new mutable.PriorityQueue[Job]()(MyOrderingRatio) ++ (li)
    val ll = pq.dequeueAll.toList
    val list_completionTime_weight = ll.foldLeft(List((0d,0d)))((acc,b) => acc.::((acc.head._1 + b.length,b.weight)))
    list_completionTime_weight.foldLeft(0d)((acc, b) => acc+b._1*b._2)
  }

  def ComputeWeightedCompletionTime(li:List[Job]): Double = {
    val pq =  new mutable.PriorityQueue[Job]()(MyOrdering) ++ (li)
    val ll = pq.dequeueAll.toList
    val list_completionTime_weight = ll.foldLeft(List((0d,0d)))((acc,b) => acc.::((acc.head._1 + b.length,b.weight)))
    list_completionTime_weight.foldLeft(0d)((acc, b) => acc+b._1*b._2)
  }

  def main(args: Array[String]) {

    val input = scala.io.Source.fromFile("src/main/scala/s1/jobs.txt", "utf-8").getLines.mkString("\n").split("\n").toList.drop(1).map(x => new Job(x.split(" ")(0).toDouble,x.split(" ")(1).toDouble))
    println(input.last)
    //val pq = new mutable.PriorityQueue[Job]()(MyOrdering) ++ (List(Job(30,90),Job(10,30),Job(20,40),Job(30,50),Job(20,60),Job(10,50)))
    println(ComputeWeightedCompletionTime(List(Job(30,90),Job(10,30),Job(20,40),Job(30,50),Job(20,60),Job(10,50))))
    println(ComputeWeightedCompletionTimeRatio(List(Job(30,90),Job(10,30),Job(20,40),Job(30,50),Job(20,60),Job(10,50))))


    println(ComputeWeightedCompletionTime(input).toString)
   System.out.println(">> "+new DecimalFormat("#0.###").format(ComputeWeightedCompletionTime(input)))
    System.out.println(">> "+new DecimalFormat("#0.###").format(ComputeWeightedCompletionTimeRatio(input)))

  }
}
