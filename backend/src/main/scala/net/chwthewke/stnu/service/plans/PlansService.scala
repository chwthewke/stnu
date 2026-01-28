package net.chwthewke.stnu
package service
package plans

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.Method.DELETE
import org.http4s.Method.GET
import org.http4s.Method.POST
import org.http4s.circe.CirceEntityCodec.*

import persistence.PlansPersistenceApi
import protocol.codec.UriCodec
import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanSummary
import protocol.persistence.PlansApi

class PlansService[F[_]]( private val data: PlansPersistenceApi[F] )( using F: Async[F] )
    extends PlansApi[F]
    with UriCodec.Dsl[F]:

  override def readPlans: F[Vector[PlanSummary]] = data.readPlans

  override def savePlan( plan: Plan, overwrite: Boolean ): F[Option[PlanId]] =
    F.realTimeInstant.flatMap( data.savePlan( plan, _, overwrite ) )

  override def readPlan( planId: PlanId ): OptionT[F, Plan] = data.readPlan( planId )

  override def deletePlan( planId: PlanId ): F[Boolean] = data.deletePlan( planId )

  val routes: HttpRoutes[F] =
    import PlansService.*
    HttpRoutes.of:
      case GET -> PA.plans()                    => Ok( readPlans )
      case req @ POST -> PA.savePlan( confirm ) => Ok( req.as[Plan].flatMap( savePlan( _, confirm ) ) )
      case GET -> PA.readPlan( id )             => readPlan( id ).foldF( NotFound() )( Ok( _ ) )
      case DELETE -> PA.readPlan( id )          => Ok( deletePlan( id ) )

object PlansService:
  val PA: PlansApi.type = PlansApi

  def apply[F[_]: Async]( data: PlansPersistenceApi[F] ): PlansService[F] = new PlansService[F]( data )
