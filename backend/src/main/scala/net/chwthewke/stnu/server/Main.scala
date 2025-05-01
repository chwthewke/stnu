package net.chwthewke.stnu
package server

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp:
  override def run( args: List[String] ): IO[ExitCode] =
    AppServer.resource[IO].use( _ => IO.pure( ExitCode.Success ) )
