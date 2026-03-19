package net.chwthewke.stnu
package protocol
package persistence

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

opaque type PlanId = Int

object PlanId:
  inline def apply( id: Int ): PlanId = id

  extension ( self: PlanId ) def id: Int = self

  given Show[PlanId]       = Show[Int]
  given Order[PlanId]      = Order[Int]
  given Ordering[PlanId]   = Ordering[Int]
  given Decoder[PlanId]    = Decoder[Int]
  given Encoder[PlanId]    = Encoder[Int]
  given KeyDecoder[PlanId] = KeyDecoder[Int]
  given KeyEncoder[PlanId] = KeyEncoder[Int]
