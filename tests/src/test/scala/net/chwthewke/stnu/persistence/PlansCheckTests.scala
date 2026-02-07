package net.chwthewke.stnu
package persistence

import cats.syntax.all.*
import java.time.Instant

import model.ClockSpeedPreset
import model.prod.FlowEnd
import model.prod.Group
import persistence.Plans.Organisation
import protocol.persistence.EndId
import protocol.persistence.Flows
import protocol.persistence.PlanId
import protocol.persistence.PlanName
import protocol.persistence.ProcessSplitId
import protocol.persistence.ProductionUi

class PlansCheckTests extends CheckTests:

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
    val organisation =
      Organisation(
        Flows(
          0x18841536,
          ProcessSplitId( 1 ),
          Vector(
            EndId.Process( ClassName( "foo" ) ) -> Vector(
              ( ProcessSplitId( 1 ), 0.4d, Group( Vector( 2, 1 ) ) ),
              ( ProcessSplitId( 2 ), 0.6d, Group( Vector.empty ) )
            )
          ),
          Map(
            ClassName( "bar" ) ->
              Vector( Vector( ( FlowEnd.Source, ProcessSplitId( 1 ) ), ( FlowEnd.Source, ProcessSplitId( 2 ) ) ) ),
            ClassName( "baz" ) ->
              Vector(
                Vector( ( FlowEnd.Destination, ProcessSplitId( 1 ) ), ( FlowEnd.Destination, ProcessSplitId( 2 ) ) )
              )
          )
        ),
        ProductionUi(
          Some( Vector( ProcessSplitId( 2 ), ProcessSplitId( 1 ) ) ),
          Vector( ProcessSplitId( 2 ) )
        )
      )

    check(
      Plans.statements
        .insertPlanOptions(
          PlanId( 1 ),
          true,
          ClassName( "machine" ),
          ClockSpeedPreset.`100%`,
          organisation
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
