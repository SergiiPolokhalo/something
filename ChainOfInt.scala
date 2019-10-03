import scala.annotation.tailrec

object ChainOfInt extends App {
  val d = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

  println(deep(d, 111))

  @tailrec
  def deep(n: List[Int], eq: Int, r: Map[String, Int] = Map()): Map[String, Int] = {
    if (n.isEmpty) r.filter(_._2 == eq)
    else {
      val nn = n.head
      if (r.isEmpty) {
        deep(n.tail, eq, Map(s"$nn" -> nn))
      } else {
        deep(n.tail, eq, r.flatMap(x =>
          List(
            s"${x._1}+$nn" -> (x._2 + nn),
            s"${x._1}-$nn" -> (x._2 - nn),
            s"${x._1}*$nn" -> (x._2 * nn),
            s"${x._1}/$nn" -> (x._2 / nn)
          )
        ))
      }
    }
  }
}
