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
      toDatatype: ToDatatype[R],
  ): ToDatatype[T] =
    new ToDatatype[T] {
      override def datatype: Datatype = toDatatype.datatype
    }

  implicit def hconsToDatatype[FieldName <: Symbol, Field, Tail <: HList](
      implicit
      witness: Witness.Aux[FieldName],
      tailToDatatype: ToDatatype[Tail],
  ): ToDatatype[FieldType[FieldName, Field] :: Tail] =
    new ToDatatype[FieldType[FieldName, Field] :: Tail] {
      override def datatype: Datatype =
        Datatype(Seq(witness.value.name) ++ tailToDatatype.datatype.fields)
    }

  implicit def hnilToDatatype: ToDatatype[HNil] =
    new ToDatatype[HNil] {
      override def datatype = Datatype(Seq.empty)
    }

  implicit def cconsToDatatype[VariantName, Variant, Tail <: Coproduct](implicit
      headToDatatype: ToDatatype[Variant],
      tailToDatatype: ToDatatype[Tail],
  ): ToDatatype[FieldType[VariantName, Variant] :+: Tail] =
    new ToDatatype[FieldType[VariantName, Variant] :+: Tail] {
      override def datatype: Datatype =
        Datatype(
          headToDatatype.datatype.fields ++ tailToDatatype.datatype.fields,
        )
    }

  implicit def cnilToDatatype: ToDatatype[CNil] =
    new ToDatatype[CNil] {
      override def datatype: Datatype = Datatype(Seq.empty)
    }

  def get[T](implicit toDatatype: ToDatatype[T]): Datatype = {
    toDatatype.datatype
  }
}

final case class Datatype(fields: Seq[String])
