package net.chwthewke.stnu
package game

import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.Decoder

case class BuildingDescriptor(
    className: ClassName,
    smallIcon: Option[IconData]
) derives Show

object BuildingDescriptor:
  given Decoder[BuildingDescriptor] =
    import Parsers.*
    Decoder.forProduct2( "ClassName", "mSmallIcon" )( BuildingDescriptor.apply )(
      Decoder[ClassName],
      texture2d.decoder.map( _.some ).or( Decoder.const( none ) )
    )
