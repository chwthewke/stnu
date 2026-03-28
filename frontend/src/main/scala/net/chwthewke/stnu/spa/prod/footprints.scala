package net.chwthewke.stnu
package spa
package prod

import cats.Semigroup
import cats.Show

import model.Footprint

object footprints:

  opaque type Row = Footprint

  object Row:
    inline def apply( footprint: Footprint ): Row = footprint

    extension ( self: Row ) def footprint: Footprint = self

    given Show[Row] = Show[Footprint]
    given Semigroup[Row]:
      override def combine( x: Row, y: Row ): Row =
        Footprint( x.length.max( y.length ), x.width + y.width )

  opaque type Floor = Footprint

  object Floor:
    inline def apply( footprint: Footprint ): Floor = footprint

    extension ( self: Floor ) def footprint: Footprint = self

    given Show[Floor] = Show[Footprint]
    given Semigroup[Floor]:
      override def combine( x: Floor, y: Floor ): Floor =
        Footprint( x.length + y.length + 800, x.width.max( y.width ) )

  opaque type Building = Footprint

  object Building:
    inline def apply( footprint: Footprint ): Building = footprint

    extension ( self: Building ) def footprint: Footprint = self

    given Show[Building] = Show[Footprint]
    given Semigroup[Building]:
      override def combine( x: Building, y: Building ): Building =
        Footprint( x.length.max( y.length ), x.width.max( y.width ) )
