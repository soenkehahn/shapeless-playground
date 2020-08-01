package example

import shapeless._
import Datatype._

object Main extends App {
  println(Datatype.get[Parent])
}

sealed trait Parent

final case class Foo(a: String, b: String) extends Parent
final case class Bar(c: String, d: String) extends Parent
