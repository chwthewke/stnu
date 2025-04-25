package net.chwthewke.stnu
package model

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder

opaque type Tier = Int

object Tier:
  inline def apply( tier: Int ): Tier     = tier
  extension ( tier: Tier ) def value: Int = tier

  val values: Vector[Tier] = ( 0 to 9 ).toVector

  given Show[Tier]     = Show.catsShowForInt
  given Order[Tier]    = Order[Int]
  given Ordering[Tier] = Order.catsKernelOrderingForOrder

  given Decoder[Tier] = Decoder.decodeInt
  given Encoder[Tier] = Encoder.encodeInt
