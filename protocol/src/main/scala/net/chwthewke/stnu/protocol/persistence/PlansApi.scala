package net.chwthewke.stnu
package protocol
package persistence

import cats.data.OptionT
import cats.~>

import protocol.codec.PathCodec
import protocol.codec.QueryCodec
import protocol.codec.SegmentCodec
import protocol.codec.UriCodec

trait PlansApi[F[_]]:
  self =>
  def readPlans: F[Vector[PlanSummary]]

  def savePlan( plan: Plan, overwrite: Boolean ): F[Option[PlanId]]

  def readPlan( planId: PlanId ): OptionT[F, Plan]

  def deletePlan( planId: PlanId ): F[Boolean]

  final def mapK[G[_]]( f: F ~> G ): PlansApi[G] =
    new PlansApi[G]:
      override def readPlans: G[Vector[PlanSummary]] = f( self.readPlans )

      override def savePlan( plan: Plan, overwrite: Boolean ): G[Option[PlanId]] = f( self.savePlan( plan, overwrite ) )

      override def readPlan( planId: PlanId ): OptionT[G, Plan] = self.readPlan( planId ).mapK( f )

      override def deletePlan( planId: PlanId ): G[Boolean] = f( self.deletePlan( planId ) )

object PlansApi:

  import PathCodec.Root
  import QueryCodec.*
  import SegmentCodec.*

  val plans: UriCodec.Constant    = Root / "api" / "plans"
  val savePlan: UriCodec[Boolean] = Root / "api" / "plan" :? singleOptWithDefault( "overwrite", false )
  val readPlan: UriCodec[PlanId]  = Root / "api" / "plan" / Int.imap( PlanId( _ ) )( _.id )
