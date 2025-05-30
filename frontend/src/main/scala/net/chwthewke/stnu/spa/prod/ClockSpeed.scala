package net.chwthewke.stnu
package spa
package prod

import cats.Order
import cats.Show

opaque type ClockSpeed = Double

object ClockSpeed:
  inline def apply( x: Double ): ClockSpeed         = x
  extension ( cs: ClockSpeed ) def toDouble: Double = cs

  given Show[ClockSpeed]     = Show.show( cs => f"$cs%3.4f" )
  given Order[ClockSpeed]    = Order[Double]
  given Ordering[ClockSpeed] = Order.catsKernelOrderingForOrder
