package net.chwthewke.stnu
package data
package newts

import cats.kernel.CommutativeMonoid

opaque type All = Boolean

object All:
  inline def apply( x: Boolean ): All         = x
  extension ( self: All ) def getAll: Boolean = self

  given CommutativeMonoid[All]:
    override def empty: All                     = true
    override def combine( x: All, y: All ): All = x && y
