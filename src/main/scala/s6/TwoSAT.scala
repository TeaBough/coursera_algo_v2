package s6

/**
 * Created with IntelliJ IDEA.
 * User: thibault
 * Date: 18/10/13
 * Time: 14:08
 * To change this template use File | Settings | File Templates.
 */
object TwoSAT {
  def TransformTwoSatInputToGraph(l: List[String]): List[String] = {
    l.flatMap(line => {
      val a = line.split(" ")(0).toInt
      val b = line.split(" ")(1).toInt
      List((a * (-1)).toString() + " " + b.toString(), (b * (-1)).toString() + " " + a.toString() )
    })
  }

  def main(args: Array[String]) {
    val input = scala.io.Source.fromFile("src/main/scala/s6/2sat6.txt", "utf-8")
    val splited_input = input.getLines.mkString("\n").split("\n")
    val n = splited_input.head.toInt
    val my_input: List[String] = splited_input.toList.drop(1)
    //println(my_input)
    val graph: List[String] = TransformTwoSatInputToGraph(my_input)
    println("Done")
  }
}
