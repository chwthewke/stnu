package net.chwthewke.stnu
package model

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder

opaque type ClockSpeed = Double

object ClockSpeed:
  inline def ofPercent( x: Double ): ClockSpeed  = x
  inline def ofFraction( x: Double ): ClockSpeed = 100d * x
  extension ( cs: ClockSpeed )
    def percent: Double  = cs
    def fraction: Double = cs / 100d

  given Show[ClockSpeed]     = Show.show( cs => f"$cs%3.4f" )
  given Order[ClockSpeed]    = Order[Double]
  given Ordering[ClockSpeed] = Order.catsKernelOrderingForOrder
  given Decoder[ClockSpeed]  = Decoder[Double]
  given Encoder[ClockSpeed]  = Encoder[Double]
