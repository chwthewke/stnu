package net.chwthewke.stnu
package persistence

import cats.syntax.all.*
import java.time.Instant

import model.ClockSpeedPreset
import protocol.persistence.PlanId
import protocol.persistence.PlanName

class PlansCheckTests extends PersistenceTests:

  test( "check SELECT plan summaries" ):
    check( Plans.statements.selectPlans )

  test( "check SELECT plan header" ):
    check( Plans.statements.selectPlanHeader( PlanId( 1 ) ) )

  test( "check SELECT plan allowed" ):
    check( Plans.statements.selectPlanAllowed( PlanId( 1 ) ) )

  test( "check SELECT plan resource nodes" ):
    check( Plans.statements.selectPlanResourceNodes( PlanId( 1 ) ) )

  test( "check SELECT plan resource options" ):
    check( Plans.statements.selectPlanResourceOptions( PlanId( 1 ) ) )

  test( "check SELECT plan requested" ):
    check( Plans.statements.selectPlanRequested( PlanId( 1 ) ) )

  test( "check INSERT plan (unconfirmed)" ):
    check( Plans.statements.insertPlan( PlanName( "plan" ), Instant.now(), confirm = false ) )

  test( "check INSERT plan (confirmed)" ):
    check( Plans.statements.insertPlan( PlanName( "plan" ), Instant.now(), confirm = true ) )

  test( "check INSERT plan options" ):
    check(
      Plans.statements
        .insertPlanOptions(
          PlanId( 1 ),
          true,
          ClassName( "machine" ),
          ClockSpeedPreset.`100%`,
          Vector( 0, 2, 3, 1 ).some
        )
    )

  test( "check DELETE plan allowed classes" ):
    check( Plans.statements.deletePlanAllowedClasses( PlanId( 1 ) ) )

  test( "check INSERT plan allowed classes" ):
    check( Plans.statements.insertPlanAllowedClasses( PlanId( 1 ) ) )

  test( "check DELETE plan resource nodes" ):
    check( Plans.statements.deletePlanResourceNodes( PlanId( 1 ) ) )

  test( "check INSERT plan resource nodes" ):
    check( Plans.statements.insertPlanResourceNodes( PlanId( 1 ) ) )

  test( "check DELETE plan resource options" ):
    check( Plans.statements.deletePlanResourceOptions( PlanId( 1 ) ) )

  test( "check INSERT plan resource options" ):
    check( Plans.statements.insertPlanResourceOptions( PlanId( 1 ) ) )

  test( "check DELETE plan requested" ):
    check( Plans.statements.deletePlanRequested( PlanId( 1 ) ) )

  test( "check INSERT plan requested" ):
    check( Plans.statements.insertPlanRequested( PlanId( 1 ) ) )

  test( "check DELETE plan" ):
    check( Plans.statements.deletePlan( PlanId( 1 ) ) )
