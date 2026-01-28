package net.chwthewke.stnu
package spa
package client

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.Async
import org.http4s.Method.POST
import org.http4s.Method.DELETE
import org.http4s.client.Client

import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanSummary
import protocol.persistence.PlansApi

class PlansClient[F[_]: Async] extends PlansApi[[a] =>> Kleisli[F, Client[F], a]] with CirceClient[F]:
  override def readPlans: Kleisli[F, Client[F], Vector[PlanSummary]] =
    expect[Vector[PlanSummary]]( PlansApi.plans() )

  override def savePlan( plan: Plan, overwrite: Boolean ): Kleisli[F, Client[F], Option[PlanId]] =
    expect[Option[PlanId]]( POST( plan, PlansApi.savePlan( overwrite ) ) )

  override def readPlan( planId: PlanId ): OptionT[[a] =>> Kleisli[F, Client[F], a], Plan] =
    OptionT( expectOption[Plan]( PlansApi.readPlan( planId ) ) )

  override def deletePlan( planId: PlanId ): Kleisli[F, Client[F], Boolean] =
    successful( DELETE( PlansApi.readPlan( planId ) ) )
