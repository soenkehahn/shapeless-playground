package example

import shapeless.{
  :+:,
  ::,
  CNil,
  Coproduct,
  HList,
  HNil,
  LabelledGeneric,
  Witness,
}
import shapeless.labelled.FieldType

object datatype {
  final case class Variant(name: String, fields: Seq[String])

  def get[T](implicit variants: Variants[T]): Seq[Variant] =
    variants.variants

  final case class Variants[T](variants: Seq[Variant]) {
    def cast[U]: Variants[U] = Variants(this.variants)
  }

  implicit def genericToDatatype[T, R](implicit
      generic: LabelledGeneric.Aux[T, R],
      variants: Variants[R],
  ): Variants[T] = variants.cast

  implicit def cconsToDatatype[
      VariantName <: Symbol,
      Variant,
      Tail <: Coproduct,
  ](implicit
      witness: Witness.Aux[VariantName],
      headFields: Fields[Variant],
      tailVariants: Variants[Tail],
  ): Variants[FieldType[VariantName, Variant] :+: Tail] =
    Variants(
      tailVariants.variants.prepended(
        Variant(witness.value.name, headFields.fields),
      ),
    )

  implicit def cnilToDatatype: Variants[CNil] = Variants(Seq.empty)

  final case class Fields[T](fields: Seq[String]) {
    def cast[U]: Fields[U] = Fields(this.fields)
  }

  implicit def genericFields[T, R](implicit
      generic: LabelledGeneric.Aux[T, R],
      fields: Fields[R],
  ): Fields[T] = fields.cast

  implicit def hconsToDatatype[FieldName <: Symbol, Field, Tail <: HList](
      implicit
      witness: Witness.Aux[FieldName],
      tailFields: Fields[Tail],
  ): Fields[FieldType[FieldName, Field] :: Tail] =
    Fields(Seq(witness.value.name) ++ tailFields.fields)

  implicit def hnilToDatatype: Fields[HNil] = Fields(Seq.empty)
}
