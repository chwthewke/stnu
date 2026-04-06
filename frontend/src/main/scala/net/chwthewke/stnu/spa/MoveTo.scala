package net.chwthewke.stnu
package spa

import cats.effect.Sync
import org.scalajs.dom
import tyrian.Cmd

object MoveTo:
  def element[F[_]: Sync]( id: String ): Cmd[F, Nothing] =
    Cmd.SideEffect( dom.document.getElementById( id ).scrollIntoView( top = true ) )

  def top[F[_]: Sync]: Cmd[F, Nothing] =
    Cmd.SideEffect( dom.window.scrollTo( 0, 0 ) )
