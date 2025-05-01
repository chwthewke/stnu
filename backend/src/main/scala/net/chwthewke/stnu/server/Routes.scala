package net.chwthewke.stnu
package server

import cats.effect.Sync
import cats.syntax.all.*
import org.http4s.Charset
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.StaticFile
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.scalatags.*

import server.api.ModelApi
import server.middleware.Cors
import server.middleware.LastModifiedMiddleware
import server.middleware.LoggingMiddleware
import server.pages.Index

class Routes[F[_]: Sync](
    private val serverConfig: ServerConfig,
    private val modelApi: ModelApi[F],
    private val corsMiddleware: Cors.T[F],
    private val loggingMiddleware: LoggingMiddleware.T[F],
    private val lastModifiedMiddleware: LastModifiedMiddleware.T[F],
    private val shutdown: F[Unit]
) extends Http4sDsl[F]:
  private val modelApiRoutes: HttpRoutes[F] =
    import org.http4s.circe.CirceEntityCodec.*
    HttpRoutes.of:
      case GET -> Root / "api" / "models" => Ok( modelApi.getModelIndex )
      case GET -> Root / "api" / "model" / IntVar( version ) =>
        modelApi.getModel( version ).cataF( NotFound(), Ok( _ ) )
      case GET -> Root / "api" / "model" / "latest" => Ok( modelApi.getLatestModel )

  private val systemRoutes: HttpRoutes[F] = HttpRoutes.of:
    case GET -> Root / "shutdown" => shutdown *> Ok()

  private val staticFileTypes: List[String] = List( ".js", ".css", ".map", ".png", ".ico" )

  private val staticRoutes: HttpRoutes[F] = HttpRoutes.of:
    case req @ GET -> "static" /: rest if staticFileTypes.exists( rest.renderString.endsWith ) =>
      StaticFile.fromResource[F]( "/" + rest.renderString, Some( req ) ).getOrElseF( NotFound() )

  private val pageRoutes: HttpRoutes[F] = HttpRoutes.of:
    case GET -> Root / "index.html" =>
      Ok( Index.page )
    case GET -> Root / "js" / "launcher.js" =>
      Ok(
        Index.launcherScript( serverConfig.frontendFlags ),
        `Content-Type`( MediaType.application.javascript, Charset.`UTF-8` )
      )

  val routes: HttpRoutes[F] =
    loggingMiddleware(
      systemRoutes <+> lastModifiedMiddleware( pageRoutes <+> corsMiddleware( modelApiRoutes <+> staticRoutes ) )
    )

object Routes:
  def apply[F[_]: Sync](
      serverConfig: ServerConfig,
      modelApi: ModelApi[F],
      corsMiddleware: Cors.T[F],
      loggingMiddleware: LoggingMiddleware.T[F],
      lastModifiedMiddleware: LastModifiedMiddleware.T[F],
      shutdown: F[Unit]
  ): HttpRoutes[F] =
    new Routes( serverConfig, modelApi, corsMiddleware, loggingMiddleware, lastModifiedMiddleware, shutdown ).routes
