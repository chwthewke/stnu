package net.chwthewke.stnu
package server

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all.*
import org.http4s.Charset
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.scalatags.*
import org.http4s.server.HttpMiddleware
import org.http4s.server.middleware.ErrorHandling
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.util.control.NonFatal

import server.middleware.Cors
import server.middleware.LastModifiedMiddleware
import server.middleware.LoggingMiddleware
import server.pages.Index
import service.game.ModelService
import service.plans.PlansService
import service.solver.SolverService

class Routes[F[_]: Sync](
    private val serverConfig: ServerConfig,
    private val modelHash: String,
    private val modelApi: ModelService[F],
    private val solverApi: SolverService[F],
    private val plansApi: PlansService[F],
    private val corsMiddleware: Cors.T[F],
    private val loggingMiddleware: LoggingMiddleware.T[F],
    private val lastModifiedMiddleware: LastModifiedMiddleware.T[F],
    private val shutdown: F[Unit]
) extends Http4sDsl[F]:

  private val systemRoutes: HttpRoutes[F] = HttpRoutes.of:
    case GET -> Root / "shutdown" => shutdown *> Ok()

  private val staticFileTypes: List[String] =
    List( ".js", ".css", ".map", ".png", ".ico", ".svg", ".json", ".ttf", ".woff", ".woff2" )

  private val staticRoutes: HttpRoutes[F] = HttpRoutes.of:
    case req @ GET -> "static" /: rest if staticFileTypes.exists( rest.renderString.endsWith ) =>
      StaticFile.fromResource[F]( "/" + rest.renderString, Some( req ) ).getOrElseF( NotFound() )

  private val pageRoutes: HttpRoutes[F] = HttpRoutes.of:
    case GET -> Root / "index.html" =>
      Ok( Index.page )
    case GET -> Root / "js" / "launcher.js" =>
      Ok(
        Index.launcherScript( serverConfig.frontendFlags + ( "cacheId" -> modelHash ) ),
        `Content-Type`( MediaType.application.javascript, Charset.`UTF-8` )
      )

  private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "HTTP.ERRORS" )

  private val errorHandling: HttpMiddleware[F] = ( svc: Kleisli[OptionT[F, *], Request[F], Response[F]] ) =>
    ErrorHandling.Custom.recoverWith[OptionT[F, *], F, Request[F]]( svc ):
      case NonFatal( e ) =>
        OptionT.liftF:
          logger.error( e )( "Error processing request" ) *>
            Sync[F].raiseError( e )

  val routes: HttpRoutes[F] =
    loggingMiddleware(
      errorHandling(
        systemRoutes
          <+> corsMiddleware( solverApi.routes <+> plansApi.routes )
          <+> pageRoutes
          <+> lastModifiedMiddleware( corsMiddleware( modelApi.routes <+> staticRoutes ) )
      )
    )

object Routes:
  def apply[F[_]: Sync](
      serverConfig: ServerConfig,
      modelHash: String,
      modelApi: ModelService[F],
      solverApi: SolverService[F],
      plansApi: PlansService[F],
      corsMiddleware: Cors.T[F],
      loggingMiddleware: LoggingMiddleware.T[F],
      lastModifiedMiddleware: LastModifiedMiddleware.T[F],
      shutdown: F[Unit]
  ): HttpRoutes[F] =
    new Routes(
      serverConfig,
      modelHash,
      modelApi,
      solverApi,
      plansApi,
      corsMiddleware,
      loggingMiddleware,
      lastModifiedMiddleware,
      shutdown
    ).routes
