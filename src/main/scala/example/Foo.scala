package example

import shapeless._

object ToDatatype {
  def datatype[T](generic: T): Datatype = ???
}

trait ToDatatype[T] {
  def datatype: Datatype
}

object Datatype {
  implicit def genericToDatatype[T, R](implicit
      generic: Generic.Aux[T, R],
      toDatatype: ToDatatype[R]
  ): ToDatatype[T] =
    new ToDatatype[T] {
      override def datatype: Datatype = toDatatype.datatype
    }

  implicit def hnilToDatatype: ToDatatype[HNil] =
    new ToDatatype[HNil] {
      override def datatype = Datatype("hnil")
    }

  def get[T](implicit toDatatype: ToDatatype[T]): Datatype = {
    toDatatype.datatype
  }
}

final case class Datatype(name: String)

// test

sealed case class Foo()

object Main extends App {
  import Datatype._
  println(Datatype.get[Foo])
}
