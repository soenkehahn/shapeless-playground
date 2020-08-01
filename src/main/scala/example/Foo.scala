package example

sealed case class Foo()

object Main extends App {
  import Datatype._
  println(Datatype.get[Foo])
}
