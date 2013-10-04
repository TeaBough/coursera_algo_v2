package s4

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 04/10/13
 * Time: 17:03
 */

case class Edge(left: Int, right: Int, weight: Int)

object Floyd_Warshall {

  def arrayToString(a: Array[Array[Double]]): String = {
    val str = for (l <- a) yield l.mkString("{", "   |   ", "}")
    str.mkString("", ",\n", "")
  }

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s4/g3.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.split(" ")(0).toInt
    val edges_nb = splited_input.head.split(" ")(1).toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    val final_input = my_input.map(x => new Edge(x.split(" ")(0).toInt, x.split(" ")(1).toInt, x.split(" ")(2).toInt))

    var c_arr = Array.ofDim[Double](n + 1, n + 1)
    var p_arr = Array.ofDim[Double](n + 1, n + 1)
    (0 to n).foreach(i => (0 to n).foreach(j => p_arr(i)(j) = if (i == j) 0 else 9999))
    final_input.foreach(x => p_arr(x.left)(x.right) = x.weight)


    var min = 9999.9
    for (k <- 1 until n)  {
      for (i <- 1 until n) {
        for (j <- 1 until n) {
          c_arr(i)(j) = Math.min(p_arr(i)(j), p_arr(i)(k) + p_arr(k)(j))
          min = Math.min(c_arr(i)(j),min)
        }
      }
      p_arr = c_arr
    }
    println(min)
  }

}


