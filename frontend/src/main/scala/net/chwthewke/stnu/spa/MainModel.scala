package net.chwthewke.stnu
package spa

import cats.effect.Async
import cats.effect.Sync
import cats.syntax.all.*
import org.scalajs.dom
import tyrian.Cmd

import model.ModelIndex
import protocol.game.FullModel
import spa.Msg.SetLocation
import spa.browse.BrowseModel
import spa.browse.BrowseMsg
import spa.library.LibraryModel
import spa.library.LibraryMsg
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
      planPage: PlanModel,
      libraryPage: LibraryModel
  )

object MainModel:
  def error[F[_]]( message: String ): ( MainModel[F], Cmd[F, Nothing] ) = ( Error( message ), Cmd.None )

  def init[F[_]: Async]( flags: Map[String, String] ): ( MainModel[F], Cmd[F, Msg] ) =
    Http.Flags
      .of( flags )
      .map( Http.init[F]( _ ) )
      .fold(
        error,
        http => Loading( http, LocationModel.init ) -> http.fetchLatestGameModel
      )

  extension [F[_]: Async]( model: MainModel[F] )
    def update( message: Msg ): ( MainModel[F], Cmd[F, Msg] ) =
      ( model, message ) match
        // INIT & DATA FETCH ("KERNEL")
        case ( _: MainModel.Error[F], _ ) => model -> Cmd.None

        case ( m: MainModel.Loading[F], Msg.RecvGameModel( index, model ) ) =>
          m.receiveGameModel( index, model )

        case ( m: MainModel.Loaded[F], Msg.RecvGameModel( index, model ) ) =>
          m.receiveGameModel( index, model )

        case ( m: MainModel.Loaded[F], Msg.FetchGameModel( version ) ) =>
          m -> m.http.fetchGameModel( m.content.modelIndex, version )

        // NAVIGATION
        case ( _, Msg.SetLocation( location ) ) =>
          model match
            case MainModel.Loading( http, _ ) => Loading( http, location ) -> Cmd.None
            case m: MainModel.Loaded[F]       => m.setLocation( location )
            case _                            => model                     -> Cmd.None

        case ( model, Msg.NavigateExternal( uri ) ) =>
          model -> Cmd.SideEffect( dom.window.open( url = uri, target = "_blank" ) )

        // PAGE MESSAGES
        case ( m: MainModel.Loaded[F], Msg.BrowseMessage( browseMsg ) ) =>
          val ( newBrowsePage: BrowseModel, browseCmd: Cmd[F, BrowseMsg] ) =
            m.browsePage.update( browseMsg )
          m.copy( browsePage = newBrowsePage ) -> browseCmd.map( Msg.BrowseMessage( _ ) )
        case ( m: MainModel.Loaded[F], Msg.PlanMessage( planMsg ) ) =>
          val ( newPlanPage: PlanModel, planCmd: Cmd[F, PlanMsg] ) =
            m.planPage.update[F]( m.http, planMsg )
          m.copy( planPage = newPlanPage ) -> planCmd.map( Msg.PlanMessage( _ ) )
        case ( m: MainModel.Loaded[F], Msg.LibraryMessage( libraryMsg ) ) =>
          val ( newLibraryPage: LibraryModel, libraryCmd: Cmd[F, LibraryMsg] ) =
            m.libraryPage.update( m.http, libraryMsg )
          m.copy( libraryPage = newLibraryPage ) -> libraryCmd.map( Msg.LibraryMessage( _ ) )

        case ( _, _ ) => model -> Cmd.None

  extension [F[_]: Sync]( model: MainModel.Loading[F] )
    def receiveGameModel( index: ModelIndex, fullModel: FullModel ): ( MainModel.Loaded[F], Cmd[F, Msg] ) =
      val contentModel = ContentModel( index, fullModel, model.http )
      MainModel.Loaded(
        model.http,
        model.location,
        contentModel,
        BrowseModel.init,
        PlanModel.init( contentModel.env ),
        LibraryModel.init( contentModel.env )
      ) -> Cmd.Emit( SetLocation( model.location ) )

  extension [F[_]: Async]( model: MainModel.Loaded[F] )
    def setLocation( location: LocationModel ): ( MainModel.Loaded[F], Cmd[F, Msg] ) =
      val ( updatedBrowseModel: Option[BrowseModel], updatedPlanModel: Option[PlanModel], command: Cmd[F, Msg] ) =
        location match
          case LocationModel.Browse =>
            ( model.browsePage.restore.some, none, Cmd.None )
          case LocationModel.Plan( options, None, organizer ) =>
            ( none, model.planPage.setTab( options, organizer ).restore.some, Cmd.None )
          case LocationModel.Plan( options, Some( id ), organizer ) =>
            (
              none,
              model.planPage.setTab( options, organizer ).restore.some,
              model.planPage.loadPlan( model.http, id ).map( Msg.PlanMessage( _ ) )
            )
          case LocationModel.Library =>
            ( none, none, LibraryModel.loadLibrary[F]( model.http ).map( Msg.LibraryMessage( _ ) ) )

      Loaded(
        model.http,
        location,
        model.content,
        updatedBrowseModel.getOrElse( model.browsePage ),
        updatedPlanModel.getOrElse( model.planPage ),
        model.libraryPage
      ) -> command

    def receiveGameModel( index: ModelIndex, fullModel: FullModel ): ( MainModel.Loaded[F], Cmd[F, Msg] ) =
      val content = ContentModel( index, fullModel, model.http )
      model.copy( content = content, planPage = PlanModel.init( content.env ) ) -> Cmd.None
      // TODO adapt page models to new content?
