package example

import shapeless._
import shapeless.labelled._

object ToDatatype {
  def datatype[T](generic: T): Datatype = ???
}

trait ToDatatype[T] {
  def datatype: Datatype
}

object Datatype {
  implicit def genericToDatatype[T, R](implicit
      generic: LabelledGeneric.Aux[T, R],
      toDatatype: ToDatatype[R]
  ): ToDatatype[T] =
    new ToDatatype[T] {
      override def datatype: Datatype = toDatatype.datatype
    }

  implicit def hconsToDatatype[K <: Symbol, H, Tail <: HList](implicit
      witness: Witness.Aux[K],
      tailToDatatype: ToDatatype[Tail]
  ): ToDatatype[FieldType[K, H] :: Tail] =
    new ToDatatype[FieldType[K, H] :: Tail] {
      override def datatype: Datatype =
        Datatype(Seq(witness.value.name) ++ tailToDatatype.datatype.fields)
    }

  implicit def hnilToDatatype: ToDatatype[HNil] =
    new ToDatatype[HNil] {
      override def datatype = Datatype(Seq.empty)
    }

  def get[T](implicit toDatatype: ToDatatype[T]): Datatype = {
    toDatatype.datatype
  }

  def getFieldName[K, V](
      value: FieldType[K, V]
  )(implicit witness: Witness.Aux[K]): K =
    witness.value
}

final case class Datatype(fields: Seq[String])
