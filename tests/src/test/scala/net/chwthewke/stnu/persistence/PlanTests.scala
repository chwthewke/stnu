package net.chwthewke.stnu
package persistence

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import doobie.implicits.*
import munit.CatsEffectSuite
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import protocol.persistence.Plan
import protocol.persistence.RequestSelection

class PlanTests extends CatsEffectSuite with TransactorFixture with ModelFixture with ScalaCheckSuite:

  given Approx[RequestSelection]:
    override def approx( x: RequestSelection, y: RequestSelection ): Boolean =
      x.requestedAmounts =~ y.requestedAmounts

  given Approx[Plan]:
    override def approx( x: Plan, y: Plan ): Boolean =
      x.name == y.name
        && x.recipeOptions == y.recipeOptions
        && x.resourceOptions == y.resourceOptions
        && x.extractionOptions == y.extractionOptions
        && x.logisticsOptions == y.logisticsOptions
        && x.powerOptions == y.powerOptions
        && x.requestSelection =~ y.requestSelection
        && x.flows == y.flows
        && x.productionUi == y.productionUi

  DataVersionStorage.cases.foreach: version =>
    test( s"write + read plan ${version.docsKey}" ):
      forAll( PlanGenerators.plan( models( version ).game )() ): plan =>
        val retrieved: Option[Plan] =
          IO.realTimeInstant
            .flatMap: now =>
              OptionT( truncate *> Plans.savePlan( plan, now, false ) )
                .flatMap( Plans.readPlan )
                .value
                .transact( transactor )
            .unsafeRunSync()

        assert( retrieved =~ plan.some )
