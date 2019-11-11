class Tree(val x:Int=0, val l:Tree=null, val r:Tree=null){
  override def toString()=s"$x"
}
val input = new Tree(
  1,
  new Tree (2, new Tree(4, new Tree(5, null, new Tree(8)))),
  new Tree (
    3, 
    new Tree(
      6,
      new Tree(9),
      new Tree(10)
    ), 
    new Tree(7)
    )
  )

import scala.annotation.tailrec
@tailrec
def deep(t:List[Tree], level:Int=0) {
  println(t)
  if (!t.isEmpty) {
    deep(
    t.map(tr => {
      if (null != tr) 
        Seq(tr.l,tr.r)
      else 
        Seq()
      }).flatten.filter(null != _),
    level + 1
    )
  }
}

deep(List(input))
