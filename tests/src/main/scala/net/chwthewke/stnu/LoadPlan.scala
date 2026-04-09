package net.chwthewke.stnu

import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path

import persistence.Codecs
import persistence.CurrentFsPlans
import persistence.PlansPersistenceApi
import protocol.persistence.Plan

object LoadPlan:
  def load[F[_]: Async]( path: Path, name: String )( using files: Files[F] ): OptionT[F, Plan] =
    val plans: PlansPersistenceApi[F] = new CurrentFsPlans[F]( path, Codecs.latest )
    OptionT( plans.readPlans.map( _.find( _.name.name == name ).map( _.planId ) ) )
      .flatMap( plans.readPlan )
