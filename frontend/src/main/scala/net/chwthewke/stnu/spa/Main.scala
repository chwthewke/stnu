package net.chwthewke.stnu
package spa

import cats.effect.IO
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.dom.FetchClientBuilder
import scala.scalajs.js.annotation.JSExportTopLevel
import tyrian.Cmd
import tyrian.Html
import tyrian.Location
import tyrian.Sub
import tyrian.TyrianIOApp
import tyrian.syntax.*

import model.Model as GameModel

// shims
case class Model( backend: Uri, game: Option[GameModel] )
sealed trait Msg
object Msg:
  case object Noop                             extends Msg
  case class RecvGameModel( model: GameModel ) extends Msg

@JSExportTopLevel( "TyrianApp" )
object Main extends TyrianIOApp[Msg, Model]:
  override def router: Location => Msg = _ => Msg.Noop

  override def init( flags: Map[String, String] ): ( Model, Cmd[IO, Msg] ) =
    val model = Model( Uri.unsafeFromString( flags( "backend" ) ), None )
    ( model, fetchGameModel( model ) )

  override def update( model: Model ): Msg => ( Model, Cmd[IO, Msg] ) =
    case Msg.Noop                       => ( model, Cmd.None )
    case Msg.RecvGameModel( gameModel ) => ( model.copy( game = Some( gameModel ) ), Cmd.None )

  override def view( model: Model ): Html[Msg] =
    Html.div(
      Html.p( Html.text( "Tyrian app started" ) ),
      Html.p( Html.text( s"Backend @ ${model.backend.renderString}" ) ),
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

  override def subscriptions( model: Model ): Sub[IO, Msg] = Sub.None

  private def fetchGameModel( model: Model ): Cmd[IO, Msg] =
    Cmd.Run(
      FetchClientBuilder[IO].create
        .expect[GameModel]( ( model.backend / "api" / "model" / "latest" ).renderString ),
      Msg.RecvGameModel( _ )
    )
