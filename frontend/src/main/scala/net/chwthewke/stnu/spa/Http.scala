package net.chwthewke.stnu
package spa

import cats.data.Kleisli
import cats.effect.Async
import cats.effect.Resource
import cats.syntax.all.*
import cats.~>
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.Middleware
import org.http4s.dom.FetchClientBuilder
import tyrian.Cmd

import model.ModelIndex
import protocol.game.ModelApi
import spa.client.ModelClient

class Http[F[_]: Async]( val backend: Uri, private val client: Client[F] ) extends Links:

  private val use: Kleisli[F, Client[F], *] ~> F =
    Resource.pure( client ).useKleisliK

  private val modelApi: ModelApi[F] = new ModelClient[F].mapK( use )

  def fetchGameModel( index: ModelIndex, version: ModelVersionId ): Cmd[F, Msg] =
    Cmd.Run( modelApi.getModel( version ).cata( Msg.Noop, Msg.RecvGameModel( index, _ ) ) )

  def fetchLatestGameModel: Cmd[F, Msg] =
    Cmd.Run( ( modelApi.getModelIndex, modelApi.getLatestModel ).mapN( Msg.RecvGameModel( _, _ ) ) )

object Http:
  def init[F[_]: Async]( backend: Uri ): Http[F] =
    new Http( backend, middleware[F]( backend )( FetchClientBuilder[F].create ) )

  private def middleware[F[_]: Async]( backend: Uri ): Middleware[F] = client =>
    val slashed: Uri = backend.withPath( backend.path.addEndsWithSlash )
    Client[F]( req => client.run( req.withUri( slashed.resolve( req.uri ) ) ) )
