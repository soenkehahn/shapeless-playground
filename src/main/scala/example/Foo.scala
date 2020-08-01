package example

import Datatype._

sealed case class Foo()

object Main extends App {
  println(Datatype.get[Foo])
}
