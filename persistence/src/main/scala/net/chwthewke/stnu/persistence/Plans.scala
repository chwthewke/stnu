package net.chwthewke.stnu
package persistence

import cats.Monoid
import cats.data.Ior
import cats.data.OptionT
import cats.derived.*
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import doobie.postgres.implicits.JavaInstantMeta
import io.circe.Json
import io.circe.derivation.ConfiguredCodec
import io.circe.syntax.*
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine
import model.Recipe
import model.ResourceDistrib
import model.ResourcePurity
import model.Transport
import protocol.persistence.ExtractionOptions
import protocol.persistence.Flows
import protocol.persistence.LogisticsOptions
import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanName
import protocol.persistence.PlanSummary
import protocol.persistence.PowerOptions
import protocol.persistence.ProcessSplitId
import protocol.persistence.ProductionUi
import protocol.persistence.RecipeOptions
import protocol.persistence.RequestSelection
import protocol.persistence.ResourceOptions

object Plans extends PlansPersistenceApi[ConnectionIO]:
  def apply[F[_]: MonadCancelThrow]( xa: Transactor[F] ): PlansPersistenceApi[F] = Plans.mapK( xa.trans )

  override def readPlans: ConnectionIO[Vector[PlanSummary]] =
    statements.selectPlans.stream.groupAdjacentRows
      .map:
        case ( id, ( name, updated ), requestedOpts ) =>
          PlanSummary( id, name, updated, requestedOpts.toVector.flattenOption )
      .compile
      .toVector

  override def savePlan( plan: Plan, at: Instant, overwrite: Boolean ): ConnectionIO[Option[PlanId]] =
    OptionT( statements.insertPlan( plan.name, at, overwrite ).option )
      .semiflatTap: planId =>
        updatePlanOptions( planId, plan ) *>
          updatePlanAllowed( planId, plan ) *>
          updatePlanResourceNodes( planId, plan ) *>
          updatePlanResourceOptions( planId, plan ) *>
          updatePlanRequested( planId, plan )
      .value

  private def updatePlanOptions( planId: PlanId, plan: Plan ): ConnectionIO[Unit] =
    statements
      .insertPlanOptions(
        planId,
        plan.recipeOptions.hideFicsmas,
        plan.extractionOptions.minerClass,
        plan.extractionOptions.clockSpeed,
        Organisation( plan.flows, plan.productionUi )
      )
      .run
      .void

  private def updatePlanAllowed( planId: PlanId, plan: Plan ): ConnectionIO[Unit] =
    def transports(
        classType: AllowedClassType,
        all: SortedSet[ClassName[Transport]],
        single: Option[ClassName[Transport]]
    ): Vector[( AllowedClassType, String, Boolean )] =
      ( all.map( ( _, false ) ).toMap ++ single.map( ( _, true ) ).toMap ).toVector.map:
        case ( t, s ) => ( classType, t.name, s )

    val allowed: Vector[( AllowedClassType, String, Boolean )] =
      plan.recipeOptions.allowedRecipes.toVector.map( cn => ( AllowedClassType.Recipe, cn.name, false ) ) ++
        plan.extractionOptions.extractors.toVector.map( et =>
          ( AllowedClassType.ExtractorType, et.toString, false )
        ) ++
        transports( AllowedClassType.Belt, plan.logisticsOptions.allBelts, plan.logisticsOptions.singleBelt ) ++
        transports(
          AllowedClassType.Pipeline,
          plan.logisticsOptions.allPipelines,
          plan.logisticsOptions.singlePipeline
        ) ++
        plan.powerOptions.allowedGenerators.map( cn => ( AllowedClassType.Generator, cn.name, false ) )

    statements.deletePlanAllowedClasses( planId ).run *>
      statements.insertPlanAllowedClasses( planId ).updateMany( allowed ).void

  private def updatePlanResourceNodes( planId: PlanId, plan: Plan ): ConnectionIO[Unit] =
    val resourceNodes: Vector[( ExtractorType, ClassName[Item], ResourcePurity, Int )] =
      ( for
        ( extractorType, items ) <- plan.resourceOptions.resourceNodes
        ( item, distrib )        <- items
        ( purity, amount )       <- distrib.foldMap( ( p, a ) => Vector( ( p, a ) ) )
      yield ( extractorType, item, purity, amount ) ).toVector

    statements.deletePlanResourceNodes( planId ).run *>
      statements.insertPlanResourceNodes( planId ).updateMany( resourceNodes ).void

  private def updatePlanResourceOptions( planId: PlanId, plan: Plan ): ConnectionIO[Unit] =
    val fracking: Map[ClassName[Item], Boolean] =
      plan.extractionOptions.preferFracking.map( ( _, true ) ).toMap
    val sliders: Map[ClassName[Item], Int]                             = plan.extractionOptions.resourceWeightSliders
    val resourceOptions: Vector[( ClassName[Item], ( Boolean, Int ) )] =
      fracking
        .alignWith( sliders ):
          case Ior.Left( f )    => ( f, 0 )
          case Ior.Right( n )   => ( false, n )
          case Ior.Both( f, n ) => ( f, n )
        .toVector

    statements.deletePlanResourceOptions( planId ).run *>
      statements.insertPlanResourceOptions( planId ).updateMany( resourceOptions ).void

  private def updatePlanRequested( planId: PlanId, plan: Plan ): ConnectionIO[Unit] =
    statements.deletePlanRequested( planId ).run *>
      statements.insertPlanRequested( planId ).updateMany( plan.requestSelection.requestedAmounts.toVector ).void

  override def deletePlan( planId: PlanId ): ConnectionIO[Boolean] =
    statements.deletePlan( planId ).option.map( _.isDefined )

  override def readPlan( planId: PlanId ): OptionT[ConnectionIO, Plan] =
    OptionT( statements.selectPlanHeader( planId ).option ).semiflatMap:
      case ( name, hideFicsmas, minerClass, clockSpeedPreset, organisation ) =>
        for
          allowed         <- statements.selectPlanAllowed( planId ).to[Vector]
          resourceNodes   <- statements.selectPlanResourceNodes( planId ).to[Vector]
          resourceOptions <- statements.selectPlanResourceOptions( planId ).to[Vector]
          requested       <- statements.selectPlanRequested( planId ).to[Vector]
        yield toPlan(
          name,
          hideFicsmas,
          minerClass,
          clockSpeedPreset,
          allowed,
          resourceNodes,
          resourceOptions,
          requested,
          organisation.getOrElse( Organisation.default )
        )

  case class Allowed(
      recipes: Set[ClassName[Recipe.Manufacturing]],
      extractors: Set[ExtractorType],
      belts: Set[( ClassName[Transport], Boolean )],
      pipelines: Set[( ClassName[Transport], Boolean )],
      generator: Set[ClassName[Machine]]
  ) derives Monoid:
    def logisticsOptions: LogisticsOptions =
      LogisticsOptions(
        belts.find( _._2 )._1F,
        pipelines.find( _._2 )._1F,
        belts.map( _._1 ).to( SortedSet ),
        pipelines.map( _._1 ).to( SortedSet )
      )

  object Allowed:
    def recipe( className: String ): Allowed =
      Allowed( Set( ClassName( className ) ), Set.empty, Set.empty, Set.empty, Set.empty )
    def extractor( name: String ): Allowed =
      Allowed( Set.empty, ExtractorType.withNameOption( name ).toSet, Set.empty, Set.empty, Set.empty )
    def belt( className: String, single: Boolean ): Allowed =
      Allowed( Set.empty, Set.empty, Set( ( ClassName( className ), single ) ), Set.empty, Set.empty )
    def pipeline( className: String, single: Boolean ): Allowed =
      Allowed( Set.empty, Set.empty, Set.empty, Set( ( ClassName( className ), single ) ), Set.empty )
    def generator( className: String ): Allowed =
      Allowed( Set.empty, Set.empty, Set.empty, Set.empty, Set( ClassName( className ) ) )

  private def toPlan(
      name: PlanName,
      hideFicsmas: Boolean,
      minerClass: ClassName[Machine],
      clockSpeedPreset: ClockSpeedPreset,
      allowedTypes: Vector[( AllowedClassType, String, Boolean )],
      resourceNodes: Vector[( ExtractorType, ClassName[Item], ResourcePurity, Int )],
      resourceOptions: Vector[( ClassName[Item], Boolean, Int )],
      requested: Vector[( ClassName[Item], Double )],
      organisation: Organisation
  ): Plan =
    val allowed: Allowed =
      allowedTypes.foldMap:
        case ( AllowedClassType.Recipe, cn, _ )          => Allowed.recipe( cn )
        case ( AllowedClassType.ExtractorType, name, _ ) => Allowed.extractor( name )
        case ( AllowedClassType.Belt, cn, single )       => Allowed.belt( cn, single )
        case ( AllowedClassType.Pipeline, cn, single )   => Allowed.pipeline( cn, single )
        case ( AllowedClassType.Generator, cn, _ )       => Allowed.generator( cn )

    val ( preferFracking: Set[ClassName[Item]], weights: Map[ClassName[Item], Int] ) =
      resourceOptions.foldMap:
        case ( item, fracking, weight ) =>
          ( Option.when( fracking )( item ).toSet, Map( item -> weight ) )

    val resourceDistributions: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]] =
      resourceNodes
        .foldMap:
          case ( extractorType, item, purity, amount ) =>
            Map( extractorType -> Map( item -> Map( purity -> amount ) ) )
        .fmap:
          _.fmap: amounts =>
            ResourceDistrib(
              amounts.getOrElse( ResourcePurity.Impure, 0 ),
              amounts.getOrElse( ResourcePurity.Normal, 0 ),
              amounts.getOrElse( ResourcePurity.Pure, 0 )
            )

    Plan(
      name,
      RecipeOptions( hideFicsmas, allowed.recipes ),
      ResourceOptions( resourceDistributions ),
      ExtractionOptions( minerClass, clockSpeedPreset, allowed.extractors, preferFracking, weights ),
      allowed.logisticsOptions,
      PowerOptions( allowed.generator ),
      RequestSelection( requested.to( SortedMap ) ),
      organisation.flows,
      organisation.productionUi
    )

  case class Organisation( flows: Flows, productionUi: ProductionUi ) derives ConfiguredCodec
  object Organisation:
    val default: Organisation =
      Organisation(
        Flows( 0, ProcessSplitId( 1 ), Vector.empty, Map.empty ),
        ProductionUi( none, Vector.empty )
      )
    given Get[Organisation] = Get[Json].temap( ( j: Json ) => j.as[Organisation].leftMap( _.show ) )
    given Put[Organisation] = Put[Json].contramap( _.asJson )

  object statements:
    val selectPlans: Query0[( PlanId, ( PlanName, Instant ), Option[( ClassName[Item], Double )] )] =
      // language=SQL
      sql"""SELECT
           |    p."id"
           |  , p."name"
           |  , p."updated"
           |  , q."item_class"
           |  , q."amount"
           |FROM      "plans"          p
           |LEFT JOIN "plan_requested" q ON p."id" = q."plan_id"
           |ORDER BY p."updated" DESC, p."id" DESC
           |""".stripMargin.query

    def selectPlanHeader(
        planId: PlanId
    ): Query0[( PlanName, Boolean, ClassName[Machine], ClockSpeedPreset, Option[Organisation] )] =
      // language=SQL
      sql"""SELECT
           |    p."name"
           |  , o."hide_ficsmas"
           |  , o."miner_class"
           |  , o."clock_speed_preset"
           |  , o."organisation"
           |FROM       "plans"        p
           |INNER JOIN "plan_options" o ON p."id" = o."plan_id"
           |WHERE p."id" = $planId
           |""".stripMargin.query

    def selectPlanAllowed( planId: PlanId ): Query0[( AllowedClassType, String, Boolean )] =
      // language=SQL
      sql"""SELECT
           |    c."type"
           |  , c."class"
           |  , c."single"
           |FROM "plan_allowed_classes" c
           |WHERE c."plan_id" = $planId
           |""".stripMargin.query

    def selectPlanResourceNodes( planId: PlanId ): Query0[( ExtractorType, ClassName[Item], ResourcePurity, Int )] =
      // language=SQL
      sql"""SELECT
           |    n."extractor_type"
           |  , n."item_class"
           |  , n."purity"
           |  , n."amount"
           |FROM "plan_resource_nodes" n
           |WHERE n."plan_id" = $planId
           |""".stripMargin.query

    def selectPlanResourceOptions( planId: PlanId ): Query0[( ClassName[Item], Boolean, Int )] =
      // language=SQL
      sql"""SELECT
           |    o."item_class"
           |  , o."prefer_fracking"
           |  , o."value"
           |FROM "plan_resource_options" o
           |WHERE o."plan_id" = $planId
           |""".stripMargin.query

    def selectPlanRequested( planId: PlanId ): Query0[( ClassName[Item], Double )] =
      // language=SQL
      sql"""SELECT
           |    q."item_class"
           |  , q."amount"
           |FROM "plan_requested" q
           |WHERE q."plan_id" = $planId
           |""".stripMargin.query

    def insertPlan( name: PlanName, updated: Instant, confirm: Boolean ): Query0[PlanId] =
      val confirmedFragment: Fragment =
        if ( confirm )
          fr0"""DO UPDATE SET "updated" = excluded."updated""""
        else
          fr0"""DO NOTHING"""
        // language=SQL
      sql"""INSERT INTO "plans" 
           |  ("name", "updated")
           |VALUES
           |  ($name, $updated)
           |ON CONFLICT ON CONSTRAINT "plans_name_unique"
           |  $confirmedFragment
           |RETURNING "plans"."id"
           |""".stripMargin.query

    def insertPlanOptions(
        planId: PlanId,
        hideFicsmas: Boolean,
        minerClass: ClassName[Machine],
        clockSpeedPreset: ClockSpeedPreset,
        organisation: Organisation
    ): Update0 =
      // language=SQL
      sql"""INSERT INTO "plan_options"
           |( "plan_id"
           |, "hide_ficsmas"
           |, "miner_class"
           |, "clock_speed_preset"
           |, "organisation"
           |)
           |VALUES
           |( $planId
           |, $hideFicsmas
           |, $minerClass
           |, $clockSpeedPreset
           |, $organisation
           |)
           |ON CONFLICT ON CONSTRAINT "plan_options_plan_unique"
           |  DO UPDATE SET
           |      "plan_id"            = excluded."plan_id"
           |    , "hide_ficsmas"       = excluded."hide_ficsmas"
           |    , "miner_class"        = excluded."miner_class"
           |    , "clock_speed_preset" = excluded."clock_speed_preset"
           |    , "organisation"       = excluded."organisation"
           |""".stripMargin.update

    def deletePlanAllowedClasses( planId: PlanId ): Update0 =
      // language=SQL
      sql"""DELETE FROM "plan_allowed_classes"
           |WHERE "plan_id" = $planId
           |""".stripMargin.update

    def insertPlanAllowedClasses( planId: PlanId ): Update[( AllowedClassType, String, Boolean )] =
      Update[( PlanId, ( AllowedClassType, String, Boolean ) )](
        // language=SQL
        """INSERT INTO "plan_allowed_classes"
          |( "plan_id"
          |, "type"
          |, "class"
          |, "single"
          |)
          |VALUES ( ?, ?, ?, ? )
          |""".stripMargin
      ).contramap( ( planId, _ ) )

    def deletePlanResourceNodes( planId: PlanId ): Update0 =
      // language=SQL
      sql"""DELETE FROM "plan_resource_nodes"
           |WHERE "plan_id" = $planId
           |""".stripMargin.update

    def insertPlanResourceNodes( planId: PlanId ): Update[( ExtractorType, ClassName[Item], ResourcePurity, Int )] =
      Update[( PlanId, ( ExtractorType, ClassName[Item], ResourcePurity, Int ) )](
        // language=SQL
        """INSERT INTO "plan_resource_nodes"
          |( "plan_id"
          |, "extractor_type"
          |, "item_class"
          |, "purity"
          |, "amount"
          |)
          |VALUES ( ?, ?, ?, ?, ? )
          |""".stripMargin
      ).contramap( ( planId, _ ) )

    def deletePlanResourceOptions( planId: PlanId ): Update0 =
      // language=SQL
      sql"""DELETE FROM "plan_resource_options"
           |WHERE "plan_id" = $planId
           |""".stripMargin.update

    def insertPlanResourceOptions( planId: PlanId ): Update[( ClassName[Item], ( Boolean, Int ) )] =
      Update[( PlanId, ( ClassName[Item], ( Boolean, Int ) ) )](
        // language=SQL
        """INSERT INTO "plan_resource_options"
          |( "plan_id"
          |, "item_class"
          |, "prefer_fracking"
          |, "value"
          |)
          |VALUES ( ?, ?, ?, ? )
          |""".stripMargin
      ).contramap( ( planId, _ ) )

    def deletePlanRequested( planId: PlanId ): Update0 =
      // language=SQL
      sql"""DELETE FROM "plan_requested"
           |WHERE "plan_id" = $planId
           |""".stripMargin.update

    def insertPlanRequested( planId: PlanId ): Update[( ClassName[Item], Double )] =
      Update[( PlanId, ( ClassName[Item], Double ) )](
        // language=SQL
        """INSERT INTO "plan_requested"
          |( "plan_id"
          |, "item_class"
          |, "amount"
          |)
          |VALUES ( ?, ?, ? )
          |""".stripMargin
      ).contramap( ( planId, _ ) )

    def deletePlan( planId: PlanId ): Query0[PlanId] =
      // language=SQL
      sql"""DELETE FROM "plans"
           |WHERE "id" = $planId
           |RETURNING "id"
           |""".stripMargin.query
