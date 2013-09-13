package s1

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 13/09/13
 * Time: 22:18
 * To change this template use File | Settings | File Templates.
 */
case class Edge(left: Int, right: Int, weight: Int)

object prism {

  def findMinEdge(vertices: mutable.Set[Int], remainingEdges: List[Edge]): (Edge, List[Edge]) = {
    def findMinEdgeRec(vertices: mutable.Set[Int], remainingEdges: List[Edge], minE: Edge): Edge = {
      if (remainingEdges.isEmpty)
        minE
      else {
        val currentEdge = remainingEdges.head
        if ((vertices.contains(currentEdge.left) ^ vertices.contains(currentEdge.right)) && currentEdge.weight < minE.weight)
          findMinEdgeRec(vertices, remainingEdges.tail, currentEdge)
        else findMinEdgeRec(vertices, remainingEdges.tail, minE)
      }
    }
    val maxWeight = remainingEdges.foldLeft(0)((acc, b) => acc.max(b.weight))
    val minEdge = findMinEdgeRec(vertices, remainingEdges, new Edge(0, 0, maxWeight + 1))
    (minEdge, remainingEdges.diff(List(minEdge)))
  }

  def main(args: Array[String]) {
    //A refactorer en plus fonctionnel !!!
    val input = scala.io.Source.fromFile("src/main/scala/s1/edges.txt", "utf-8").getLines.mkString("\n").split("\n").toList
    val nbOfVertices = input.head.split(" ")(0).toInt
    var graph = input.drop(1).map(x => new Edge(x.split(" ")(0).toInt,x.split(" ")(1).toInt,x.split(" ")(2).toInt ))
    val v0 = graph.head.left
    val explored = mutable.Set(v0)
    var sum:BigInt = 0
    val minGraph = mutable.Set[Edge]()
    while (explored.size != nbOfVertices) {
      val min = findMinEdge(explored, graph)
      minGraph.add(min._1)
      sum = sum + min._1.weight
      explored.add(min._1.left)
      explored.add(min._1.right)
      graph = min._2
    }
    println(sum)
  }

}
