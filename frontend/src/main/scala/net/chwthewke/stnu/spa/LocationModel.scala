package net.chwthewke.stnu
package spa

import org.http4s.Uri
import tyrian.Location
import tyrian.Routing

enum LocationModel:
  case Browse
  case Plan

object LocationModel extends Enum[LocationModel]:
  val init: LocationModel = Browse
  val router: Location => Msg =
    Routing.basic(
      href => parseInternalHref( href ).fold( Msg.Noop )( Msg.SetLocation( _ ) ),
      Msg.NavigateExternal( _ )
    )

  // reciprocal of toInternalLocation
  private def parseInternalHref( href: String ): Option[LocationModel] =
    Uri
      .fromString( href )
      .toOption
      .flatMap( _.fragment )
      .flatMap( _.split( '!' ).lift( 1 ) )
      .flatMap( LocationModel.withNameOption )

  extension ( location: LocationModel )
    def toInternalLocation: String =
      s"#!$location"
