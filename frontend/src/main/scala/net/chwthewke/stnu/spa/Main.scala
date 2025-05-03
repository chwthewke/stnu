package net.chwthewke.stnu
package spa

import cats.effect.Async
import cats.effect.IO
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.client.Middleware
import org.http4s.dom.FetchClientBuilder
import scala.scalajs.js.annotation.JSExportTopLevel
import tyrian.Cmd
import tyrian.Html
import tyrian.Location
import tyrian.Sub
import tyrian.TyrianIOApp
import tyrian.syntax.*

import css.Bulma
import model.Model as GameModel
import protocol.game.ModelApi

// shims
case class Model( http: Http, game: Option[GameModel] )

case class Http( backend: Uri ):
  def middleware[F[_]: Async]: Middleware[F] = client =>
    val slashed: Uri = backend.withPath( backend.path.addEndsWithSlash )
    Client[F]( req => client.run( req.withUri( slashed.resolve( req.uri ) ) ) )

  def client[F[_]: Async]: Client[F] =
    middleware[F]( FetchClientBuilder[F].create )

sealed trait Msg
object Msg:
  case object Noop                             extends Msg
  case class RecvGameModel( model: GameModel ) extends Msg

@JSExportTopLevel( "TyrianApp" )
object Main extends TyrianIOApp[Msg, Model]:
  val bp: Bulma = Bulma

  override def router: Location => Msg = _ => Msg.Noop

  override def init( flags: Map[String, String] ): ( Model, Cmd[IO, Msg] ) =
    val model = Model( Http( Uri.unsafeFromString( flags( "backend" ) ) ), None )
    ( model, fetchGameModel( model ) )

  override def update( model: Model ): Msg => ( Model, Cmd[IO, Msg] ) =
    case Msg.Noop                       => ( model, Cmd.None )
    case Msg.RecvGameModel( gameModel ) => ( model.copy( game = Some( gameModel ) ), Cmd.None )

  override def view( model: Model ): Html[Msg] =
    Html.div(
      Html.article( bc( bp.message, bp.isInfo ) )(
        Html.div( bc( bp.messageHeader ) )(
          Html.p( Html.text( "Tyrian app started" ) )
        ),
        Html.div( bc( bp.messageBody ) )(
          Html.p( Html.text( s"Backend @ ${model.http.backend.renderString}" ) ),
          model.game
            .map( gm =>
              Html.p(
                Html.text(
                  s"Model loaded with ${gm.items.size} items and ${gm.manufacturingRecipes.size} manufacturing recipes!"
                )
              )
            )
            .orEmpty
        )
      )
    )

  override def subscriptions( model: Model ): Sub[IO, Msg] = Sub.None

  private def fetchGameModel( model: Model ): Cmd[IO, Msg] =
    Cmd.Run(
      model.http
        .client[IO]
        .expect[GameModel]( ModelApi.getLatestModel() ),
      Msg.RecvGameModel( _ )
    )
