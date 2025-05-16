package net.chwthewke.stnu
package spa

import cats.effect.IO
import cats.syntax.all.*
import scala.scalajs.js.annotation.JSExportTopLevel
import tyrian.CSS
import tyrian.Cmd
import tyrian.Html
import tyrian.TyrianApp
import tyrian.TyrianIOApp

import spa.css.Bulma
import spa.saved.LocalModel

@JSExportTopLevel( "DevTyrianApp" )
object MainWithHotReload
    extends HotReloadApp[IO, Msg, MainModel[IO], LocalModel.Saved, LocalModel.Loaded]
    with TyrianIOApp[HotReloadApp.Msg[Msg, LocalModel.Loaded], HotReloadApp.Model[MainModel[IO], LocalModel.Loaded]]:
  override val delegate: TyrianApp[IO, Msg, MainModel[IO]] = Main

  override def save( appModel: MainModel[IO] ): Option[LocalModel.Saved] =
    appModel match
      case m: MainModel.Loaded[IO] => LocalModel.Saved( m ).some
      case _                       => none

  override def encode( saved: LocalModel.Saved ): String = LocalModel.Saved.encode( saved )

  override def decode( src: Option[String] ): Either[String, LocalModel.Loaded] = LocalModel.Loaded.decode( src )

  override def loadInto( appModel: MainModel[IO], loaded: LocalModel.Loaded ): Option[( MainModel[IO], Cmd[IO, Msg] )] =
    loaded.loadInto( appModel )

  override def view(
      model: HotReloadApp.Model[MainModel[IO], LocalModel.Loaded]
  ): Html[HotReloadApp.Msg[Msg, LocalModel.Loaded]] =
    val b: Bulma = Bulma
    import views.given
    Html.div( b.themeDark )(
      super.view( model ),
      Html
        .div(
          b.notification + b.isDanger + b.p0,
          Html.styles( CSS.position( "fixed" ), CSS.bottom( "0" ), CSS.right( "0" ) )
        )( "Dev build" )
    )
