package net.chwthewke.stnu
package persistence

import cats.Order
import cats.Show

opaque type SchemaVersion = Int

object SchemaVersion:
  inline def apply( v: Int ): SchemaVersion = v

  extension ( self: SchemaVersion ) def version: Int = self

  given Show[SchemaVersion]     = Show[Int]
  given Order[SchemaVersion]    = Order[Int]
  given Ordering[SchemaVersion] = Ordering[Int]
