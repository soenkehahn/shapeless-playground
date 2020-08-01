package example

import shapeless._
import shapeless.labelled._

object Datatype {
  implicit def genericToDatatype[T, R](implicit
      generic: LabelledGeneric.Aux[T, R],
      toDatatype: Datatype[R],
  ): Datatype[T] = toDatatype.cast

  implicit def hconsToDatatype[FieldName <: Symbol, Field, Tail <: HList](
      implicit
      witness: Witness.Aux[FieldName],
      tailToDatatype: Datatype[Tail],
  ): Datatype[FieldType[FieldName, Field] :: Tail] =
    Datatype(Seq(witness.value.name) ++ tailToDatatype.fields)

  implicit def hnilToDatatype: Datatype[HNil] = Datatype(Seq.empty)

  implicit def cconsToDatatype[VariantName, Variant, Tail <: Coproduct](implicit
      headToDatatype: Datatype[Variant],
      tailToDatatype: Datatype[Tail],
  ): Datatype[FieldType[VariantName, Variant] :+: Tail] =
    Datatype(
      headToDatatype.fields ++ tailToDatatype.fields,
    )

  implicit def cnilToDatatype: Datatype[CNil] = Datatype(Seq.empty)

  def get[T](implicit toDatatype: Datatype[T]): Datatype[T] =
    toDatatype
}

final case class Datatype[T](fields: Seq[String]) {
  def cast[U]: Datatype[U] = Datatype(this.fields)
}
