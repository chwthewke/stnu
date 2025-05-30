package net.chwthewke.stnu
package spa

import cats.syntax.all.*
import org.http4s.Uri
import tyrian.Location
import tyrian.Routing

import protocol.codec.PathCodec
import protocol.codec.SegmentCodec
import protocol.codec.UriCodec
import spa.plan.OptionsTab

enum LocationModel:
  case Browse
  case Plan( options: Option[OptionsTab] )

object LocationModel:
  val init: LocationModel     = Browse
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
      .flatMap( Uri.fromString( _ ).toOption )
      .flatMap( uriCodec.unapply )

  extension ( location: LocationModel )
    def toInternalLocation: String = s"#!${uriCodec( location ).renderString}"
    def description: String        = location match
      case LocationModel.Browse    => "Browse"
      case LocationModel.Plan( _ ) => "Plan"

    def asBrowse: Option[LocationModel.Browse.type] =
      location match
        case b: LocationModel.Browse.type => b.some
        case _                            => none
    def asPlan: Option[LocationModel.Plan] =
      location match
        case p: LocationModel.Plan => p.some
        case _                     => none

  private val uriCodec: UriCodec[LocationModel] =
    val planOptions: SegmentCodec[OptionsTab] =
      SegmentCodec.String.imapFilter( OptionsTab.withNameOption )( OptionsTab.keyOf )

    val browse: PathCodec[LocationModel.Browse.type] =
      ( PathCodec.Empty / "browse" ).as( LocationModel.Browse )

    val plan: PathCodec[LocationModel.Plan] =
      ( PathCodec.Empty / "plan" / planOptions.optional )
        .imap[LocationModel.Plan]( LocationModel.Plan( _ ) )( _.options )

    ( browse || plan ).imap( _.merge ):
      case b: Browse.type => Left( b )
      case p: Plan        => Right( p )
