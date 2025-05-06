package net.chwthewke.stnu
package server

import cats.data.OptionT
import cats.effect.Async
import cats.effect.Deferred
import cats.effect.Resource
import cats.effect.kernel.DeferredSource
import cats.syntax.all.*
import fs2.io.net.Network
import java.time.temporal.ChronoUnit
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.*
import scala.concurrent.duration.*

import server.middleware.Cors
import server.middleware.LastModifiedMiddleware
import server.middleware.LoggingMiddleware
import service.game.ModelService

class AppServer[F[_]: Async](
    val config: ServerConfig,
    val routes: HttpRoutes[F],
    private val shutdown: DeferredSource[F, Unit]
):
  def resource: Resource[F, Unit] =
    given Network[F] = Network.forAsync[F]
    EmberServerBuilder
      .default[F]
      .withHost( config.listenAddress )
      .withPort( config.listenPort )
      .withHttpApp( routes.orNotFound )
      .withShutdownTimeout( 1.second )
      .build
      .flatMap( _ => Resource.make( shutdown.get )( _ => Async[F].unit ) )

object AppServer:
  private def lastModifiedMiddleware[F[_]]( using
      F: Async[F]
  ): Resource[F, LastModifiedMiddleware.T[F]] =
    Resource
      .eval( F.realTimeInstant )
      .map: now =>
        LastModifiedMiddleware[F]( _ => OptionT.pure( now.truncatedTo( ChronoUnit.SECONDS ) ) )

  def resource[F[_]: Async]: Resource[F, Unit] =
    for
      config                 <- Resource.eval( ConfigSource.default.loadF[F, AppConfig]() )
      modelApi               <- Resource.eval( ModelService.load[F] )
      lastModifiedMiddleware <- lastModifiedMiddleware[F]
      shutdown               <- Resource.eval( Deferred[F, Unit] )
      server <- new AppServer(
                  config.server,
                  Routes(
                    config.server,
                    modelApi,
                    Cors[F],
                    LoggingMiddleware[F]( config.logging ),
                    lastModifiedMiddleware,
                    shutdown.complete( () ).void
                  ),
                  shutdown
                ).resource
    yield server
