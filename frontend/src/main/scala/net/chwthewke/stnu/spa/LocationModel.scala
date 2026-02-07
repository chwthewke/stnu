package net.chwthewke.stnu
package spa

import cats.syntax.all.*
import org.http4s.Uri
import tyrian.Location
import tyrian.Routing

import protocol.codec.PathCodec
import protocol.codec.QueryCodec
import protocol.codec.SegmentCodec
import protocol.codec.UriCodec
import protocol.persistence.PlanId
import spa.plan.SidePanel

enum LocationModel:
  case Browse
  case Plan( options: Option[SidePanel], id: Option[PlanId], organizer: Boolean = false )
  case Library

object LocationModel:
  val init: LocationModel     = Browse
  val initPlan: LocationModel = Plan( none, none )
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
      .flatMap( LocationUriCodec.unapply )

  extension ( location: LocationModel )
    def toInternalLocation: String = s"#!${LocationUriCodec( location ).renderString}"
    def description: String        = location match
      case LocationModel.Browse  => "Browse"
      case _: LocationModel.Plan => "Plan"
      case LocationModel.Library => "My Plans"

    def asBrowse: Option[LocationModel.Browse.type] =
      location match
        case b: LocationModel.Browse.type => b.some
        case _                            => none
    def asPlan: Option[LocationModel.Plan] =
      location match
        case p: LocationModel.Plan => p.some
        case _                     => none
    def asLibrary: Option[LocationModel.Library.type] =
      location match
        case l: LocationModel.Library.type => l.some
        case _                             => none

  object LocationUriCodec extends UriCodec[LocationModel]:
    import PathCodec.*
    import QueryCodec.*

    val requestsPath: String = "requests"

    val organizerPath: String             = "organizer"
    val planOrganizer: SegmentCodec[Unit] = SegmentCodec.const( organizerPath )

    val planOptions: SegmentCodec[SidePanel] =
      SegmentCodec.String.imapFilter( SidePanel.withNameOption )( SidePanel.keyOf )

    val browse: UriCodec.Constant = PathCodec.Empty / "browse"

    private val planIdQueryParam: QueryCodec[Option[PlanId]] =
      singleOpt[Int]( "id" ).imap( _.map( PlanId( _ ) ), _.map( _.id ) )

    // NOTE helping IntelliJ along with the type inference
    private val rawPlan: UriCodec[( Option[Unit], Option[SidePanel], Option[PlanId] )] =
      ( PathCodec.Empty / "plan" / planOrganizer.optional / planOptions.optional ) :? planIdQueryParam

    val plan: UriCodec[LocationModel.Plan] =
      rawPlan
        .imap[LocationModel.Plan]( {
          case ( org, opts, id ) => LocationModel.Plan( opts, id, org.isDefined )
        } )( plan => ( Option.when( plan.organizer )( () ), plan.options, plan.id ) )

    val library: UriCodec.Constant = PathCodec.Empty / "library"

    override def extract( uri: Uri ): Option[LocationModel] =
      uri match
        case browse()  => Browse.some
        case plan( p ) => p.some
        case library() => Library.some
        case _         => none

    override def build( model: LocationModel ): Uri =
      model match
        case LocationModel.Browse  => browse()
        case p: LocationModel.Plan => plan( p )
        case LocationModel.Library => library()
