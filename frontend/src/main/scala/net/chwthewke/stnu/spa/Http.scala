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
import org.scalajs.dom.console
import scala.util.control.NonFatal
import tyrian.Cmd

import model.ModelIndex
import protocol.game.ModelApi
import protocol.solver.SolverApi
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse
import spa.client.ModelClient
import spa.client.SolverClient

class Http[F[_]: Async]( val backend: Uri, private val client: Client[F] ) extends Links:

  private val use: Kleisli[F, Client[F], *] ~> F =
    Resource.pure( client ).useKleisliK

  private val modelApi: ModelApi[F]   = new ModelClient[F].mapK( use )
  private val solverApi: SolverApi[F] = new SolverClient[F].mapK( use )

  private def logError( e: Throwable, prefix: String = "" ): F[Unit] =
    ( e, prefix ).tailRecM:
      case ( t, p ) =>
        Async[F]
          .delay( console.error( p + t.getMessage ) )
          .as( Option( e.getCause ).tupleRight( "Caused by: " ).toLeft( () ) )

  private def run[M]( command: F[M] ): Cmd[F, M] =
    Cmd.Run( command.onError { case NonFatal( e ) => logError( e ) } )

  def fetchGameModel( index: ModelIndex, version: ModelVersionId ): Cmd[F, Msg] =
    run( modelApi.getModel( version ).cata( Msg.Noop, Msg.RecvGameModel( index, _ ) ) )

  def fetchLatestGameModel: Cmd[F, Msg] =
    run( ( modelApi.getModelIndex, modelApi.getLatestModel ).mapN( Msg.RecvGameModel( _, _ ) ) )

  def computeSolution( solverRequest: SolverRequest ): Cmd[F, SolverResponse] =
    run( solverApi.solve( solverRequest ) )

object Http:
  def init[F[_]: Async]( backend: Uri ): Http[F] =
    new Http( backend, middleware[F]( backend )( FetchClientBuilder[F].create ) )

  private def middleware[F[_]: Async]( backend: Uri ): Middleware[F] = client =>
    val slashed: Uri = backend.withPath( backend.path.addEndsWithSlash )
    Client[F]( req => client.run( req.withUri( slashed.resolve( req.uri ) ) ) )
