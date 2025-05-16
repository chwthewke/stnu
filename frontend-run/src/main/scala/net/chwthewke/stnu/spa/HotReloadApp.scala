package net.chwthewke.stnu
package spa

import cats.effect.Async
import cats.syntax.all.*
import scala.concurrent.duration.*
import tyrian.Cmd
import tyrian.HotReload
import tyrian.Html
import tyrian.Location
import tyrian.Sub
import tyrian.TyrianApp

abstract class HotReloadApp[F[_]: Async, AppMsg, AppModel, Saved, Loaded]
    extends TyrianApp[F, HotReloadApp.Msg[AppMsg, Loaded], HotReloadApp.Model[AppModel, Loaded]]:

  private type Model = HotReloadApp.Model[AppModel, Loaded]
  private type Msg   = HotReloadApp.Msg[AppMsg, Loaded]

  val delegate: TyrianApp[F, AppMsg, AppModel]

  def save( appModel: AppModel ): Option[Saved]
  def encode( saved: Saved ): String

  def decode( src: Option[String] ): Either[String, Loaded]
  def loadInto( appModel: AppModel, loaded: Loaded ): Option[( AppModel, Cmd[F, AppMsg] )]

  private def saveCommand( appModel: AppModel ): Cmd[F, Nothing] =
    save( appModel ).fold( Cmd.None ): saved =>
      HotReload.snapshot( HotReloadApp.key, saved, encode )

  private def saveSubscription: Sub[F, HotReloadApp.Msg[Nothing, Nothing]] =
    Sub.every( 1.second, HotReloadApp.key ).as( HotReloadApp.Msg.SaveModel )

  private def attemptLoading( model: Model ): ( Model, Cmd[F, Msg] ) =
    model.loadedModel
      .flatMap: loaded =>
        loadInto( model.appModel, loaded )
      .fold( model -> Cmd.None ):
        case ( newModel, command ) =>
          HotReloadApp.Model( newModel, None ) -> command.map( HotReloadApp.Msg.Passthrough( _ ) )

  private def updateModel( model: Model )( appMessage: AppMsg ): ( Model, Cmd[F, Msg] ) =
    val ( appUdated, appCmd )    = delegate.update( model.appModel )( appMessage )
    val ( loadedModel, loadCmd ) = attemptLoading( model.copy( appModel = appUdated ) )
    loadedModel -> ( loadCmd |+| appCmd.map( HotReloadApp.Msg.Passthrough( _ ) ) )

  override def router: Location => HotReloadApp.Msg[AppMsg, Nothing] = location =>
    HotReloadApp.Msg.Passthrough( delegate.router( location ) )

  override def init( flags: Map[String, String] ): ( Model, Cmd[F, Msg] ) =
    val ( initAppModel, initAppCmd ) = delegate.init( flags )
    HotReloadApp.Model( initAppModel, None ) ->
      ( initAppCmd.map( HotReloadApp.Msg.Passthrough( _ ) ) |+|
        HotReload.bootstrap[F, Loaded, HotReloadApp.Msg[Nothing, Loaded]]( HotReloadApp.key, decode ):
          case Left( msg )     => HotReloadApp.Msg.LoadKo( msg )
          case Right( loaded ) => HotReloadApp.Msg.LoadOk( loaded ) )

  override def update( model: Model ): Msg => ( Model, Cmd[F, Msg] ) =
    case HotReloadApp.Msg.Passthrough( appMessage ) =>
      updateModel( model )( appMessage )
    case HotReloadApp.Msg.SaveModel =>
      ( model, saveCommand( model.appModel ) )
    case HotReloadApp.Msg.LoadKo( error ) =>
      ( model, Cmd.SideEffect( Async[F].delay( org.scalajs.dom.console.log( s"Hot-reload error: $error" ) ) ) )
    case HotReloadApp.Msg.LoadOk( loaded ) =>
      attemptLoading( model.copy( loadedModel = loaded.some ) )

  override def view( model: HotReloadApp.Model[AppModel, Loaded] ): Html[HotReloadApp.Msg[AppMsg, Loaded]] =
    delegate.view( model.appModel ).map( HotReloadApp.Msg.Passthrough( _ ) )

  override def subscriptions( model: Model ): Sub[F, Msg] =
    delegate.subscriptions( model.appModel ).map( HotReloadApp.Msg.Passthrough( _ ) ) |+| saveSubscription

object HotReloadApp:
  private val key: String = "HotReloadApp.key"

  case class Model[M, L]( appModel: M, loadedModel: Option[L] )

  enum Msg[+M, +L]:
    case Passthrough( msg: M )
    case LoadOk( loaded: L )
    case LoadKo( error: String )
    case SaveModel
