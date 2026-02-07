package net.chwthewke.stnu
package data
package newts

import cats.kernel.CommutativeMonoid

opaque type Any = Boolean

object Any:
  inline def apply( b: Boolean ): Any         = b
  extension ( self: Any ) def getAny: Boolean = self
  def unapply( self: Any ): Some[Boolean]     = Some( self.getAny )

  given CommutativeMonoid[Any]:
    override def empty: Any                     = false
    override def combine( x: Any, y: Any ): Any = x || y
