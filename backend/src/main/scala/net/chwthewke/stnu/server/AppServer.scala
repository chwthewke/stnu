package net.chwthewke.stnu
package server

import cats.data.NonEmptyVector
import cats.data.OptionT
import cats.effect.Async
import cats.effect.Deferred
import cats.effect.Resource
import cats.effect.kernel.DeferredSource
import cats.syntax.all.*
import fs2.io.file.Path
import fs2.io.net.Network
import java.time.temporal.ChronoUnit
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.*
import scala.concurrent.duration.*

import model.ModelIndex
import persistence.FsPlans
import persistence.Plans
import persistence.PlansPersistenceApi
import protocol.game.FullModel
import server.middleware.Cors
import server.middleware.LastModifiedMiddleware
import server.middleware.LoggingMiddleware
import service.game.ModelService
import service.plans.PlansService
import service.solver.SolverService

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

  private def loadModels[F[_]: Async]: F[( ModelIndex, NonEmptyVector[FullModel] )] =
    for
      modelIndex <- assets.loadModelIndex[F]
      models     <- modelIndex.versions.toVector.traverse: version =>
                  ( assets.loadModel[F]( version ), assets.loadIconIndex[F]( version ) )
                    .mapN( FullModel( _, _ ) )
      modelsNev <- models.toNev.liftTo[F]( Error( "No model was loaded - index empty" ) )
    yield ( modelIndex, modelsNev )

  private def storage[F[_]: Async](
      args: List[String],
      config: persistence.Config
  ): Resource[F, PlansPersistenceApi[F]] =
    OptionT
      .fromOption[Resource[F, *]]( args.headOption )
      .cataF(
        persistence.Resources.managedTransactor( config ).map( Plans( _ ) ),
        dir => Resource.eval( FsPlans.init[F]( Path( dir ) ) )
      )

  def resource[F[_]: Async]( args: List[String] ): Resource[F, Unit] =
    for
      config                 <- Resource.eval( ConfigSource.default.loadF[F, AppConfig]() )
      lastModifiedMiddleware <- lastModifiedMiddleware[F]
      shutdown               <- Resource.eval( Deferred[F, Unit] )
      ( modelIndex, models ) <- Resource.eval( loadModels[F] )
      modelHash              <- Resource.eval( assets.loadModelHash[F] )
      plans                  <- storage( args, config.database )
      server                 <- new AppServer(
                  config.server,
                  Routes(
                    config.server,
                    modelHash,
                    ModelService( modelIndex, models ),
                    SolverService( models.toVector.map( _.game ) ),
                    PlansService( plans ),
                    Cors[F],
                    LoggingMiddleware[F]( config.logging ),
                    lastModifiedMiddleware,
                    shutdown.complete( () ).void
                  ),
                  shutdown
                ).resource
    yield server
