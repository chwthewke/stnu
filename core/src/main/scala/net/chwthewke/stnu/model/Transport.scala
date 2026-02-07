package net.chwthewke.stnu
package model

import cats.Order
import cats.Show
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class Transport(
    className: ClassName[Transport],
    displayName: String,
    perMinute: Int
) derives ConfiguredDecoder,
      ConfiguredEncoder

object Transport:
  given Show[Transport]     = Show.show( _.displayName )
  given Order[Transport]    = Order.by( t => ( t.perMinute, t.className, t.displayName ) )
  given Ordering[Transport] = Order.catsKernelOrderingForOrder
