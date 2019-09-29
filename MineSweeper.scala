import scala.annotation.tailrec
import scala.io.StdIn
case class Field(x: String, y: String, minen: Boolean = false, open: Boolean = false, flagged: Boolean = false)
object MineSweeper extends App {
  def init(): List[Field] = List(
    Field("0", "0"), Field("0", "1", true),
    Field("1", "0"), Field("1", "1"),
  )
  //implement action of user
  def shoowField(prior: List[Field]) = println(prior);
  def action(prior: List[Field]): List[Field] = {
    shoowField(prior);
    val input = StdIn.readLine().split(" ").map(_.trim).toList
    input match {
      case List("o", x, y) => open(x, y, prior)
      case List("m", x, y) => mark(x, y, prior)
      case List("q", _, _) => Nil
      case _ => Nil
    }
  }
  def count(fields: List[Field]): Int = fields.foldLeft(0)((acc, f) => acc + { if (f.minen) 1 else 0 })
  def splitBy(x: String, y: String, fields: List[Field]): (List[Field], List[Field]) = fields.partition(f => !(f.x == x && f.y == y))
  def open(x: String, y: String, fields: List[Field]): List[Field] = {
    val (l, r) = splitBy(x, y, fields)
    l ::: r.map(_.copy(open = true))
  }
  def mark(x: String, y: String, fields: List[Field]): List[Field] = {
    val (l, r) = splitBy(x, y, fields)
    l ::: r.map(x => x.copy(flagged = !x.flagged))
  }
  def allFound(fields: List[Field]): Boolean = fields.count(f => f.flagged && f.minen) == count(fields)
  def boom(fields: List[Field]): Boolean = fields.count(f => f.open && f.minen) > 0

  @tailrec
  def game(playground: List[Field], stat: (Int, Int, Int) = (0, 0, 0)): (Int, Int, Int) = {
    val after = action(playground)
    if (after.isEmpty || boom(after) || allFound(after)) stat
    else game(after, (stat._1 + 1, stat._2, stat._3))
  }
  game(init())
}
