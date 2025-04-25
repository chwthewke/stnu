package net.chwthewke.stnu
package model

import cats.Order
import cats.Show
import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class Item(
    className: ClassName,
    displayName: String,
    form: Form,
    fuelValue: Double,
    sinkPoints: Int
) derives ConfiguredDecoder,
      ConfiguredEncoder

object Item:
  given showItem: Show[Item] = Show.show: item =>
    show"""${item.displayName} # ${item.className}
          |Form: ${item.form}
          |Energy: ${item.fuelValue} MJ
          |Sink: ${item.sinkPoints} points
          |""".stripMargin

  given Order[Item]    = Order.by( _.displayName )
  given Ordering[Item] = Order.catsKernelOrderingForOrder
