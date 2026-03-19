package net.chwthewke.stnu
package persistence

import cats.data.OptionT
import cats.~>
import java.time.Instant

import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanSummary

trait PlansPersistenceApi[F[_]]:
  self =>
  def readPlans: F[Vector[PlanSummary]]

  def savePlan( plan: Plan, at: Instant, overwrite: Boolean ): F[Option[PlanId]]

  def readPlan( planId: PlanId ): OptionT[F, Plan]

  def deletePlan( planId: PlanId ): F[Boolean]

  final def mapK[G[_]]( f: F ~> G ): PlansPersistenceApi[G] =
    new PlansPersistenceApi[G]:
      override def readPlans: G[Vector[PlanSummary]] = f( self.readPlans )

      override def savePlan( plan: Plan, at: Instant, overwrite: Boolean ): G[Option[PlanId]] =
        f( self.savePlan( plan, at, overwrite ) )

      override def readPlan( planId: PlanId ): OptionT[G, Plan] = self.readPlan( planId ).mapK( f )

      override def deletePlan( planId: PlanId ): G[Boolean] = f( self.deletePlan( planId ) )
