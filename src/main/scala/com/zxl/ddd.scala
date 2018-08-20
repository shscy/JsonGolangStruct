package com.zxl
// 这只是一个玩具演示代码

trait BaseClass {
  val Name:String
  trait API {
    val api = "api"
  }
}

trait Child extends BaseClass{
  def b = "fff"
  trait API[T] extends super.API{
    override val api = "ggg"
    trait BB[T]{
      val c=  "ffff"
      def z[A](a: List[A]) = a.size
    }
    type a = BB[Int]
  }
  val api = new API[Int]{}

}

object Child2 extends Child{
  def aa = "fff"
  override val Name= "fffg"
}


class Parent {
  def log={
    println("base")
  }
}

class TestChild extends Parent{
  def logChild(): Unit ={
    println("child")
  }
}

