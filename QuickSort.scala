def qs(a:Seq[String]):Seq[String] = {
  println("... sorting")
  if(a.length <=1) a
  else {
    val pivot = a(a.length /2)
    Seq.concat (
      qs(a.filter(pivot > )),
        a.filter( pivot == ),
      qs(a.filter(pivot < ))  
      )
  }
}

val r = () => qs( "Undefined Order long String".toList.map(""+_) )

def sort(doIt:Boolean, sorter: () => Seq[String]):Seq[String] ={
  if (doIt) sorter() else Seq[String]()
}

println(sort(false, r) )
println(sort(true, r) )
