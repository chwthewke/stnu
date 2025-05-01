package net.chwthewke.stnu
package server
package middleware

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all.*
import java.time.Instant
import java.time.temporal.ChronoUnit
import org.http4s.CacheDirective
import org.http4s.HttpDate
import org.http4s.HttpRoutes
import org.http4s.Method.GET
import org.http4s.Method.HEAD
import org.http4s.Request
import org.http4s.Response
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Expires
import org.http4s.headers.`Cache-Control`
import org.http4s.headers.`If-Modified-Since`
import org.http4s.headers.`Last-Modified`
import org.http4s.server.HttpMiddleware
import scala.concurrent.duration.*

object LastModifiedMiddleware:

  type T[F[_]] <: HttpMiddleware[F]

  def apply[F[_]: Async]( getLastModified: PartialFunction[Request[F], OptionT[F, Instant]] ): T[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val defaultNone: Request[F] => OptionT[F, Instant] = Function.const( OptionT.none )
    val cacheControl = `Cache-Control`( CacheDirective.`must-revalidate`, CacheDirective.`max-age`( 24.hours ) )

    def getNotModified( req: Request[F], lastModified: Instant ): Option[Unit] =
      req.headers
        .get[`If-Modified-Since`]
        .filter: ifModifiedSince =>
          !ifModifiedSince.date.toInstant.isBefore( lastModified )
        .void

    def adaptResponse( response: OptionT[F, Response[F]], lastModified: Instant ): OptionT[F, Response[F]] =
      response
        .semiflatMap: resp =>
          (
            HttpDate
              .fromInstant( lastModified )
              .liftTo[F],
            Async[F].realTimeInstant
              .map( _.plus( 24, ChronoUnit.HOURS ) )
              .flatMap( HttpDate.fromInstant( _ ).liftTo[F] )
          )
            .mapN: ( date, in24Hours ) =>
              resp.putHeaders( cacheControl, `Last-Modified`( date ), Expires( in24Hours ) )

    def processSafeRequest( req: Request[F] )( routes: HttpRoutes[F] ): F[Option[Response[F]]] =
      getLastModified
        .applyOrElse( req, defaultNone )
        .cataF(
          routes( req ).value,
          lastModified =>
            getNotModified( req, lastModified )
              .fold( adaptResponse( routes( req ), lastModified ).value )( _ => NotModified().map( _.some ) )
        )

    val middleware: HttpMiddleware[F] =
      routes =>
        Kleisli:
          case req @ ( GET | HEAD ) -> _ => OptionT( processSafeRequest( req )( routes ) )
          case req                       => routes( req )

    middleware.asInstanceOf[T[F]]
