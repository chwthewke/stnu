package net.chwthewke.stnu
package spa

import cats.effect.Async
import cats.effect.IO
import scala.scalajs.js.annotation.JSExportTopLevel
import tyrian.Cmd
import tyrian.Html
import tyrian.Location
import tyrian.Sub
import tyrian.TyrianApp
import tyrian.TyrianIOApp

import spa.views.MainView

abstract class Main[F[_]: Async] extends TyrianApp[F, Msg, MainModel[F]]:
  override def router: Location => Msg = LocationModel.router

  override def init( flags: Map[String, String] ): ( MainModel[F], Cmd[F, Msg] ) = MainModel.init[F]( flags )

  override def update( model: MainModel[F] ): Msg => ( MainModel[F], Cmd[F, Msg] ) = model.update

  override def view( model: MainModel[F] ): Html[Msg] = MainView.view( model )

  override def subscriptions( model: MainModel[F] ): Sub[F, Msg] = Sub.None

@JSExportTopLevel( "TyrianApp" )
object Main extends Main[IO] with TyrianIOApp[Msg, MainModel[IO]]
