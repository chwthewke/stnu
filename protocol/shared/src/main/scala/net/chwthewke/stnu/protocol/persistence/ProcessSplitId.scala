package net.chwthewke.stnu
package protocol
package persistence

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder

opaque type ProcessSplitId = Int

object ProcessSplitId:
  inline def apply( id: Int ): ProcessSplitId = id
  extension ( self: ProcessSplitId )
    def id: Int                     = self
    def +( n: Int ): ProcessSplitId = id + n

  given Show[ProcessSplitId]     = Show[Int]
  given Order[ProcessSplitId]    = Order[Int]
  given Ordering[ProcessSplitId] = Ordering[Int]
  given Decoder[ProcessSplitId]  = Decoder[Int]
  given Encoder[ProcessSplitId]  = Encoder[Int]
