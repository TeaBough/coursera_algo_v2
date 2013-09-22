package s2

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 21/09/13
 * Time: 18:23
 * To change this template use File | Settings | File Templates.
 */

case class Edge(left: Int, right: Int, weight: Int)

object Cluster {


  def MyOrdering = new Ordering[Edge] {
    def compare(a: Edge, b: Edge) = {
      a.weight.compare(b.weight)
    }
  }

  def uniontize(l: List[Edge], u: UnionFind[Int]): Int = {
    if (u.sizeUnion == 4) {
      l.find(x => !u.find(x.left, x.right)) match {
        case Some(e) => e.weight
        case None => -1
      }
    }
    else {
      val left = l.head.left
      val right = l.head.right
      if (u.find(left, right))
        uniontize(l.tail, u)
      else {
        u.union(left, right)
        uniontize(l.tail, u)
      }
    }
  }


  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s2/clustering1.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val vertices_number = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    val final_input = my_input.map(x => new Edge(x.split(" ")(0).toInt, x.split(" ")(1).toInt, x.split(" ")(2).toInt))

    val ordered_edges = final_input.sorted(MyOrdering)

    val ds = new UnionFind[Int]((1 until vertices_number+1).toArray)

    println("Answer : " + uniontize(ordered_edges, ds))


  }

}
