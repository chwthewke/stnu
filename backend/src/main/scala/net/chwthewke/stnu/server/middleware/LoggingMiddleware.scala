package net.chwthewke.stnu
package server.middleware

import cats.effect.Async
import org.http4s.Request
import org.http4s.server.HttpMiddleware
import org.http4s.server.middleware.RequestLogger
import org.http4s.server.middleware.ResponseLogger
import pureconfig.ConfigReader
import pureconfig.generic.semiauto.deriveReader

object LoggingMiddleware:

  type T[F[_]] <: HttpMiddleware[F]

  case class Config(
      logRequest: Boolean,
      logRequestBody: Boolean,
      logResponse: Boolean,
      logResponseBody: Boolean
  )

  object Config:
    given ConfigReader[Config] = deriveReader

  def apply[F[_]: Async]( config: Config ): T[F] =
    def requestLogger: HttpMiddleware[F] =
      RequestLogger.httpRoutes( logHeaders = true, logBody = config.logRequestBody )
    def responseLogger: HttpMiddleware[F] =
      ResponseLogger.httpRoutes( logHeaders = true, logBody = config.logResponseBody )

    def identityOr( log: Boolean, middleware: => HttpMiddleware[F] ): HttpMiddleware[F] =
      if ( log ) middleware else identity

    val middleware: HttpMiddleware[F] =
      identityOr( config.logRequest, requestLogger ) andThen identityOr( config.logResponse, responseLogger )

    middleware.asInstanceOf[T[F]]
