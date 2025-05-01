package net.chwthewke.stnu
package server.middleware

import cats.Monad
import org.http4s.HttpRoutes
import org.http4s.Method
import org.http4s.headers.*
import org.http4s.server.HttpMiddleware
import org.http4s.server.middleware.CORS as Http4sCors
import org.http4s.server.middleware.CORSPolicy
import org.typelevel.ci.CIString
import scala.concurrent.duration.*

object Cors:
  type T[F[_]] <: HttpMiddleware[F]

  private val allowedMethods: Set[Method] = Set( Method.GET, Method.POST )

  private val allowedHeaders: Set[CIString] = Set(
    `Accept-Encoding`.headerInstance,
    `Authorization`.headerInstance,
    `Content-Type`.headerInstance,
    `Content-Length`.headerInstance
  ).map( _.name )

  private val policy: CORSPolicy = Http4sCors.policy
    .withMaxAge( 10.minutes )
    .withAllowMethodsIn( allowedMethods )
    .withAllowHeadersIn( allowedHeaders )

  def apply[F[_]: Monad]: T[F] = ( ( service: HttpRoutes[F] ) => policy( service ) ).asInstanceOf[T[F]]
