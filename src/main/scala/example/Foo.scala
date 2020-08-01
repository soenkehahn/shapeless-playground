package example

import shapeless._
import Datatype._

sealed case class Foo(a: String, b: String)

object Main extends App {
  println(Datatype.get[Foo])
}
