package net.chwthewke.stnu

import cats.Order
import cats.Show
import cats.derived.strict.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

opaque type ModelVersionId = Int
object ModelVersionId:
  inline def apply( id: Int ): ModelVersionId              = id
  extension ( modelVersionId: ModelVersionId ) def id: Int = modelVersionId

  given Show[ModelVersionId]     = Show[Int]
  given Order[ModelVersionId]    = Order[Int]
  given Ordering[ModelVersionId] = Order.catsKernelOrderingForOrder

  given Decoder[ModelVersionId] = Decoder[Int]
  given Encoder[ModelVersionId] = Encoder[Int]

case class ModelVersion( version: ModelVersionId, name: String, key: String ) derives Show, ConfiguredDecoder, ConfiguredEncoder

object ModelVersion:
  given Order[ModelVersion]    = Order.by( _.version )
  given Ordering[ModelVersion] = Order.catsKernelOrderingForOrder
