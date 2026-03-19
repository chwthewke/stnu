package net.chwthewke.stnu
package protocol
package persistence

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

opaque type PlanName = String

object PlanName:
  inline def apply( name: String ): PlanName = name

  extension ( self: PlanName ) def name: String = self

  given Show[PlanName]       = Show[String]
  given Order[PlanName]      = Order[String]
  given Ordering[PlanName]   = Ordering[String]
  given Decoder[PlanName]    = Decoder[String]
  given Encoder[PlanName]    = Encoder[String]
  given KeyDecoder[PlanName] = KeyDecoder[String]
  given KeyEncoder[PlanName] = KeyEncoder[String]
