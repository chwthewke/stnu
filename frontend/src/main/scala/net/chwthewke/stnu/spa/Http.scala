package net.chwthewke.stnu
package spa

import cats.data.Kleisli
import cats.data.ValidatedNel
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.Middleware
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom.console
import scala.util.control.NonFatal
import tyrian.Cmd

import model.ModelIndex
import protocol.game.ModelApi
import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanSummary
import protocol.persistence.PlansApi
import protocol.solver.SolverApi
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse
import spa.client.ModelClient
import spa.client.PlansClient
import spa.client.SolverClient

class Http[F[_]: Async]( private val flags: Http.Flags, private val client: Client[F] ) extends Links:

  def backend: Uri = flags.backend

  private val modelApi: ModelApi[F] =
    new ModelClient[F].mapK( Kleisli.applyK( Http.cacheIdMiddleware( flags.cacheId )( client ) ) )
  private val solverApi: SolverApi[F] = new SolverClient[F].mapK( Kleisli.applyK( client ) )
  private val plansApi: PlansApi[F]   = new PlansClient[F].mapK( Kleisli.applyK( client ) )

  private def logError( e: Throwable ): F[Unit] =
    Async[F].delay( console.error( e.getMessage ) )

  private def run[M]( command: F[M] ): Cmd[F, M] =
    Cmd.Run( command.onError { case NonFatal( e ) => logError( e ) } )

  def fetchGameModel( index: ModelIndex, version: ModelVersionId ): Cmd[F, Msg] =
    run( modelApi.getModel( version ).cata( Msg.Noop, Msg.RecvGameModel( index, _ ) ) )

  def fetchLatestGameModel: Cmd[F, Msg] =
    run( ( modelApi.getModelIndex, modelApi.getLatestModel ).mapN( Msg.RecvGameModel( _, _ ) ) )

  def computeSolution( solverRequest: SolverRequest ): Cmd[F, SolverResponse] =
    run( solverApi.solve( solverRequest ) )

  def save( plan: Plan, confirm: Boolean ): Cmd[F, Option[PlanId]] =
    run( plansApi.savePlan( plan, confirm ) )

  def loadPlan( planId: PlanId ): Cmd[F, Option[Plan]] =
    run( plansApi.readPlan( planId ).value )

  def loadLibrary: Cmd[F, Vector[PlanSummary]] =
    run( plansApi.readPlans )

  def deletePlan( planId: PlanId ): Cmd[F, Boolean] =
    run( plansApi.deletePlan( planId ) )

object Http:
  case class Flags( backend: Uri, cacheId: String )
  object Flags:
    def of( map: Map[String, String] ): Either[String, Flags] =
      def get( k: String ): ValidatedNel[String, String] = map.get( k ).toValidNel( s"Missing flag '$k'" )
      (
        get( "backend" ).andThen( Uri.fromString( _ ).leftMap( _.message ).toValidatedNel ),
        get( "cacheId" )
      ).mapN( Flags( _, _ ) )
        .leftMap( _.mkString_( ", " ) )
        .toEither

  def init[F[_]: Async]( flags: Flags ): Http[F] =
    new Http( flags, middleware[F]( flags.backend )( FetchClientBuilder[F].create ) )

  case class ModRequest[F[_]: MonadCancelThrow]( f: Request[F] => Request[F] ) extends ( Client[F] => Client[F] ):
    override def apply( client: Client[F] ): Client[F] = Client[F]( req => client.run( f( req ) ) )

  private def middleware[F[_]: Async]( backend: Uri ): Middleware[F] = client =>
    val slashed: Uri = backend.withPath( backend.path.addEndsWithSlash )
    Client[F]( req => client.run( req.withUri( slashed.resolve( req.uri ) ) ) )

  private def cacheIdMiddleware[F[_]: MonadCancelThrow]( cacheId: String ): Middleware[F] =
    ModRequest( req => req.withUri( req.uri.withQueryParam( "v", cacheId ) ) )
