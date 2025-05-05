package net.chwthewke.stnu
package spa.client

import cats.data.Kleisli
import cats.effect.kernel.Async
import io.circe.Decoder
import org.http4s.Method.GET
import org.http4s.Request
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl

trait CirceClient[F[_]] extends Http4sClientDsl[F]:
  given Async[F] = compiletime.deferred

  def expect[A: Decoder]( uri: Uri ): Kleisli[F, Client[F], A] =
    Kleisli( _.expect[A]( uri ) )
  def expect[A: Decoder]( request: Request[F] ): Kleisli[F, Client[F], A] =
    Kleisli( _.expect[A]( request ) )

  def expectOption[A: Decoder]( uri: Uri ): Kleisli[F, Client[F], Option[A]] =
    Kleisli( _.expectOption[A]( GET( uri ) ) )
  def expectOption[A: Decoder]( request: Request[F] ): Kleisli[F, Client[F], Option[A]] =
    Kleisli( _.expectOption[A]( request ) )
