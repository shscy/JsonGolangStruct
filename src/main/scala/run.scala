import com.zxl.{TestChild, Parent}

object run extends App {
  def ff (f: (TestChild)=>Parent) {
    val c = f(new TestChild)
  }
  val gg:Function1[Parent, TestChild] = (name:Parent)=>{
    println("name: " + name)
    new TestChild
  }
  val vv = gg.isInstanceOf
  println(vv)
  ff(gg)

}