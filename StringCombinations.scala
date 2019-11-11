def combinator(args:List[String]*) = {
  
  def mrg(a:List[List[String]], b:List[List[String]]) = {
    a.flatMap(aL => b.map(bL => aL:::bL))
  }
  
  def l2ls(s:List[String]):List[List[String]]={
    s.map(List(_))
  }
  
  def l2lsr(s:List[List[String]], r:List[List[String]]=List(List())):List[List[String]] = {
    if(s.isEmpty) r
    else {
      l2lsr(s.tail,mrg(r,l2ls(s.head)))
    }
  }
  
  l2lsr(args.toList)
}

val res =  combinator(List("M","F","22","rrr"),List("Y","Xzena"),List("Noooo","Yesss"))
println(
  (res.size, res)
)
