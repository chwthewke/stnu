package net.chwthewke.stnu

import cats.Order
import cats.Show
import cats.derived.strict.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class ModelVersion( version: Int, name: String, key: String ) derives Show, ConfiguredDecoder, ConfiguredEncoder

object ModelVersion:
  given Order[ModelVersion]    = Order.by( _.version )
  given Ordering[ModelVersion] = Order.catsKernelOrderingForOrder
