package net.chwthewke.stnu
package spa

import cats.effect.Async
import cats.effect.Sync
import cats.syntax.all.*
import org.http4s.Uri
import org.scalajs.dom
import tyrian.Cmd

import model.ModelIndex
import protocol.game.FullModel
import spa.browse.BrowseModel
import spa.browse.BrowseMsg
import spa.plan.PlanModel
import spa.plan.PlanMsg

enum MainModel[F[_]]:
  case Error( message: String )
  case Loading( http: Http[F], location: LocationModel )
  case Loaded(
      http: Http[F],
      location: LocationModel,
      content: ContentModel,
      browsePage: BrowseModel,
      planPage: PlanModel
  )

object MainModel:
  def error[F[_]]( message: String ): ( MainModel[F], Cmd[F, Nothing] ) = ( Error( message ), Cmd.None )

  def init[F[_]: Async]( flags: Map[String, String] ): ( MainModel[F], Cmd[F, Msg] ) =
    flags
      .get( "backend" )
      .toRight( "Missing flag 'backend'" )
      .flatMap( Uri.fromString( _ ).leftMap( _.message ) )
      .map( Http.init[F]( _ ) )
      .fold(
        error,
        http => Loading( http, LocationModel.init ) -> http.fetchLatestGameModel
      )

  extension [F[_]: Sync]( model: MainModel[F] )
    def update( message: Msg ): ( MainModel[F], Cmd[F, Msg] ) =
      ( model, message ) match
        // INIT & DATA FETCH ("KERNEL")
        case ( MainModel.Error( _ ), _ ) => model -> Cmd.None

        case ( m @ MainModel.Loading( _, _ ), Msg.RecvGameModel( index, model ) ) =>
          m.receiveGameModel( index, model )

        case ( m @ MainModel.Loaded( _, _, _, _, _ ), Msg.RecvGameModel( index, model ) ) =>
          m.receiveGameModel( index, model )

        case ( m @ MainModel.Loaded( http, _, content, _, _ ), Msg.FetchGameModel( version ) ) =>
          m -> http.fetchGameModel( content.modelIndex, version )

        // NAVIGATION
        case ( _, Msg.SetLocation( location ) ) =>
          model match
            case MainModel.Loading( http, _ )          => Loading( http, location ) -> Cmd.None
            case m @ MainModel.Loaded( _, _, _, _, _ ) => m.setLocation( location ) -> Cmd.None
            case _                                     => model                     -> Cmd.None

        case ( model, Msg.NavigateExternal( uri ) ) =>
          model -> Cmd.SideEffect( dom.window.open( url = uri, target = "_blank" ) )

        // PAGE MESSAGES
        case ( m @ MainModel.Loaded( _, _, _, _, _ ), Msg.BrowseMessage( browseMsg ) ) =>
          val ( newBrowsePage: BrowseModel, browseCmd: Cmd[F, BrowseMsg] ) =
            m.browsePage.update( browseMsg )
          m.copy( browsePage = newBrowsePage ) -> browseCmd.map( Msg.BrowseMessage( _ ) )
        case ( m @ MainModel.Loaded( _, _, _, _, _ ), Msg.PlanMessage( planMsg ) ) =>
          val ( newPlanPage: PlanModel, planCmd: Cmd[F, PlanMsg] ) =
            m.planPage.update( planMsg )
          m.copy( planPage = newPlanPage ) -> planCmd.map( Msg.PlanMessage( _ ) )

        case ( _, _ ) => model -> Cmd.None

  extension [F[_]: Sync]( model: MainModel.Loading[F] )
    def receiveGameModel( index: ModelIndex, fullModel: FullModel ): ( MainModel.Loaded[F], Cmd[F, Msg] ) =
      MainModel.Loaded(
        model.http,
        model.location,
        ContentModel( index, fullModel, model.http ),
        BrowseModel.init,
        PlanModel.init( fullModel.game )
      ) -> Cmd.None

  extension [F[_]: Sync]( model: MainModel.Loaded[F] )
    def setLocation( location: LocationModel ): MainModel.Loaded[F] =
      Loaded(
        model.http,
        location,
        model.content,
        if ( location == LocationModel.Browse ) model.browsePage.restore else model.browsePage,
        if ( location == LocationModel.Plan ) model.planPage.restore else model.planPage
      )
    def receiveGameModel( index: ModelIndex, fullModel: FullModel ): ( MainModel.Loaded[F], Cmd[F, Msg] ) =
      model.copy( content = ContentModel( index, fullModel, model.http ) ) -> Cmd.None // TODO adapt page models to new content
