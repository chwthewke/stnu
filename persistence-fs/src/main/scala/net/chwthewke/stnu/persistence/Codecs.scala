package net.chwthewke.stnu
package persistence

import cats.InvariantSemigroupal
import cats.syntax.all.*
import fs2.io.file.Path
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scodec.Attempt
import scodec.Codec
import scodec.Err
import scodec.codecs

import data.Countable
import model.ClockSpeed
import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine
import model.Recipe
import model.ResourceDistrib
import model.Transport
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.EndId
import protocol.persistence.ExtractionOptions
import protocol.persistence.Flows
import protocol.persistence.ItemFlows
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
import protocol.solver.BoostedRecipe
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

trait Codecs:
  type P // Plan
  type S // PlanSummary

  def version: SchemaVersion
  def planName: Codec[PlanName]
  def planSummary: Codec[S]
  def plan: Codec[P]

  def getPlanName( plan: P ): PlanName
  def getSummaryUpdated( summary: S ): Instant
  def toPlanSummary( planId: PlanId, plan: P, updated: Instant ): S

object Codecs:
  type Aux[P1, S1] = Codecs { type P = P1; type S = S1 }
  val latest: Codecs.Aux[Plan, PlanSummary] = Codecs.v4

  private given InvariantSemigroupal[Codec]:
    override def imap[A, B]( fa: Codec[A] )( f: A => B )( g: B => A ): Codec[B] = fa.xmap( f, g )
    override def product[A, B]( fa: Codec[A], fb: Codec[B] ): Codec[( A, B )]   = fa :: fb

  val schemaVersion: Codec[SchemaVersion] = codecs.uint16.xmap( SchemaVersion( _ ), _.version )

  object v1 extends Codecs:
    case class Flows(
        prodHash: Int,
        nextId: ProcessSplitId,
        endSplits: Vector[( v3.V3EndId, Vector[( ProcessSplitId, Double /* fraction */, Group )] )],
        itemFlows: Map[ClassName[Item], Vector[Vector[( FlowEnd, ProcessSplitId )]]]
    )

    case class P(
        name: PlanName,
        recipeOptions: RecipeOptions,
        resourceOptions: ResourceOptions,
        extractionOptions: v3.V3ExtractionOptions,
        logisticsOptions: LogisticsOptions,
        powerOptions: v3.V3PowerOptions,
        requestSelection: RequestSelection,
        flows: Flows,
        productionUi: ProductionUi
    )

    type S = PlanSummary

    override def version: SchemaVersion = SchemaVersion( 1 )

    override def getPlanName( plan: P ): PlanName = plan.name

    override def getSummaryUpdated( summary: PlanSummary ): Instant = v2.getSummaryUpdated( summary )

    override def toPlanSummary( planId: PlanId, plan: P, updated: Instant ): PlanSummary =
      PlanSummary( planId, plan.name, updated, plan.requestSelection.requestedAmounts.toVector )

    override def planName: Codec[PlanName] = v2.planName

    override def planSummary: Codec[PlanSummary] = v2.planSummary

    override def plan: Codec[P] =
      (
        planName,
        v2.recipeOptions,
        v2.resourceOptions,
        v2.extractionOptions,
        v2.logisticsOptions,
        v2.powerOptions,
        v2.requestSelection,
        flows,
        v2.productionUi
      ).imapN( P.apply )( Tuple.fromProductTyped )

    private[Codecs] val recipeOptions: Codec[RecipeOptions]              = v2.recipeOptions
    private[Codecs] val resourceOptions: Codec[ResourceOptions]          = v2.resourceOptions
    private[Codecs] val extractionOptions: Codec[v3.V3ExtractionOptions] = v2.extractionOptions
    private[Codecs] val logisticsOptions: Codec[LogisticsOptions]        = v2.logisticsOptions
    private[Codecs] val powerOptions: Codec[v3.V3PowerOptions]           = v2.powerOptions
    private[Codecs] val requestSelection: Codec[RequestSelection]        = v2.requestSelection
    private[Codecs] val productionUi: Codec[ProductionUi]                = v2.productionUi
    private[Codecs] val endId: Codec[v3.V3EndId]                         = v2.endId
    private[Codecs] val processSplitId: Codec[ProcessSplitId]            = v2.processSplitId
    private[Codecs] val flowEnd: Codec[FlowEnd]                          = v2.flowEnd
    private[Codecs] val group: Codec[Group]                              = v2.group
    private[Codecs] def className[A]: Codec[ClassName[A]]                = v2.className[A]

    val flows: Codec[Flows] =
      val endSplits: Codec[Vector[( v3.V3EndId, Vector[( ProcessSplitId, Double, Group )] )]] =
        codecs.vectorOfN(
          codecs.uint16,
          endId :: codecs.vectorOfN( codecs.int32, processSplitId :: codecs.double :: group )
        )
      val itemFlows: Codec[Map[ClassName[Item], Vector[Vector[( FlowEnd, ProcessSplitId )]]]] =
        codecs
          .vectorOfN(
            codecs.int32,
            className[Item] ::
              codecs.vectorOfN( codecs.int32, codecs.vectorOfN( codecs.int32, flowEnd :: processSplitId ) )
          )
          .xmap( _.toMap, _.toVector )
      ( codecs.int32, processSplitId, endSplits, itemFlows ).imapN( Flows.apply )( Tuple.fromProductTyped )

  object v2 extends Codecs:
    case class P(
        name: PlanName,
        recipeOptions: RecipeOptions,
        resourceOptions: ResourceOptions,
        extractionOptions: v3.V3ExtractionOptions,
        logisticsOptions: LogisticsOptions,
        powerOptions: v3.V3PowerOptions,
        requestSelection: RequestSelection,
        flows: v3.V3Flows,
        productionUi: ProductionUi
    )
    type S = PlanSummary

    override def version: SchemaVersion = SchemaVersion( 2 )

    override def planName: Codec[PlanName] = v3.planName

    override def planSummary: Codec[PlanSummary] = v3.planSummary

    override def plan: Codec[P] = (
      planName,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requestSelection,
      flows,
      productionUi
    ).imapN( P.apply )( Tuple.fromProductTyped )

    override def getPlanName( plan: P ): PlanName = plan.name

    override def getSummaryUpdated( summary: PlanSummary ): Instant = v3.getSummaryUpdated( summary )

    override def toPlanSummary( planId: PlanId, plan: P, updated: Instant ): PlanSummary =
      PlanSummary( planId, plan.name, updated, plan.requestSelection.requestedAmounts.toVector )

    private[Codecs] val recipeOptions: Codec[RecipeOptions]              = v3.recipeOptions
    private[Codecs] val resourceOptions: Codec[ResourceOptions]          = v3.resourceOptions
    private[Codecs] val extractionOptions: Codec[v3.V3ExtractionOptions] = v3.extractionOptions
    private[Codecs] val logisticsOptions: Codec[LogisticsOptions]        = v3.logisticsOptions
    private[Codecs] val powerOptions: Codec[v3.V3PowerOptions]           = v3.powerOptions
    private[Codecs] val requestSelection: Codec[RequestSelection]        = v3.requestSelection
    private[Codecs] val flows: Codec[v3.V3Flows]                         = v3.flows
    private[Codecs] val productionUi: Codec[ProductionUi]                = v3.productionUi
    private[Codecs] val endId: Codec[v3.V3EndId]                         = v3.endId
    private[Codecs] val processSplitId: Codec[ProcessSplitId]            = v3.processSplitId
    private[Codecs] val flowEnd: Codec[FlowEnd]                          = v3.flowEnd
    private[Codecs] val group: Codec[Group]                              = v3.group
    private[Codecs] def className[A]: Codec[ClassName[A]]                = v3.className[A]

  object v3 extends Codecs:
    type S = PlanSummary

    case class V3SolverRequest(
        modelVersion: ModelVersionId,
        requested: Vector[Countable[Double, ClassName[Item]]],
        recipeSelection: Set[ClassName[Recipe.NonExtraction]],
        resources: Map[ClassName[Item], SolverRequest.Resource]
    )

    case class V3SolverResponseSolution(
        inputs: Vector[Countable[Double, ClassName[Item]]],
        recipes: Vector[Countable[Double, ClassName[Recipe.NonExtraction]]]
    )

    case class V3ExtractionOptions(
        minerClass: ClassName[Machine],
        clockSpeed: ClockSpeedPreset.Extraction,
        extractors: Set[ExtractorType],
        preferFracking: Set[ClassName[Item]],
        resourceWeightSliders: Map[ClassName[Item], Int]
    )

    case class V3PowerOptions(
        allowedGenerators: Set[ClassName[Machine]]
    )

    enum V3EndId:
      case Process( recipe: ClassName[Recipe] )
      case Input( item: ClassName[Item] )
      case Requested( item: ClassName[Item] )
      case Byproduct( item: ClassName[Item] )

    case class V3Flows(
        prodHash: Int,
        nextId: ProcessSplitId,
        endSplits: Vector[( V3EndId, Vector[( ProcessSplitId, Double /* fraction */, Group )] )],
        itemFlows: Map[ClassName[Item], ItemFlows]
    )

    case class P(
        name: PlanName,
        modelVersionId: ModelVersionId,
        recipeOptions: RecipeOptions,
        resourceOptions: ResourceOptions,
        extractionOptions: V3ExtractionOptions,
        logisticsOptions: LogisticsOptions,
        powerOptions: V3PowerOptions,
        requestSelection: RequestSelection,
        solution: Option[( V3SolverRequest, V3SolverResponseSolution )],
        flows: V3Flows,
        productionUi: ProductionUi
    )

    override def version: SchemaVersion = SchemaVersion( 3 )

    override def planName: Codec[PlanName] = v4.planName

    override def planSummary: Codec[PlanSummary] = v4.planSummary

    private[Codecs] val recipeOptions: Codec[RecipeOptions]       = v4.recipeOptions
    private[Codecs] val resourceOptions: Codec[ResourceOptions]   = v4.resourceOptions
    private[Codecs] val logisticsOptions: Codec[LogisticsOptions] = v4.logisticsOptions
    private[Codecs] val requestSelection: Codec[RequestSelection] = v4.requestSelection
    private[Codecs] val productionUi: Codec[ProductionUi]         = v4.productionUi
    private[Codecs] val processSplitId: Codec[ProcessSplitId]     = v4.processSplitId
    private[Codecs] val flowEnd: Codec[FlowEnd]                   = v4.flowEnd
    private[Codecs] val group: Codec[Group]                       = v4.group
    private[Codecs] def className[A]: Codec[ClassName[A]]         = v4.className[A]

    val extractionOptions: Codec[V3ExtractionOptions] =
      (
        className[Machine],
        v4.enumName( ClockSpeedPreset.Extraction ),
        v4.setOfN( codecs.uint8, v4.extractorType ),
        v4.setOfN( codecs.uint16, className[Item] ),
        v4.resourceWeightSliders
      ).imapN( V3ExtractionOptions.apply )( Tuple.fromProductTyped )

    val powerOptions: Codec[V3PowerOptions] =
      v4.setOfN( codecs.uint8, className[Machine] ).xmap( V3PowerOptions( _ ), _.allowedGenerators )

    val solverRequest: Codec[V3SolverRequest] =
      (
        v4.modelVersionId,
        codecs.vectorOfN( codecs.uint16, v4.countableDouble( className[Item] ) ),
        codecs.vectorOfN( codecs.uint16, className[Recipe.NonExtraction] ).imap( _.toSet )( _.toVector ),
        codecs.vectorOfN( codecs.uint8, className[Item] :: v4.requestResource ).imap( _.toMap )( _.toVector )
      ).imapN( V3SolverRequest.apply )( Tuple.fromProductTyped )

    val solverResponse: Codec[V3SolverResponseSolution] =
      (
        codecs.vectorOfN( codecs.uint16, v4.countableDouble( className[Item] ) ),
        codecs.vectorOfN( codecs.int16, v4.countableDouble( className[Recipe.NonExtraction] ) )
      ).imapN( V3SolverResponseSolution.apply )( Tuple.fromProductTyped )

    val solution: Codec[Option[( V3SolverRequest, V3SolverResponseSolution )]] =
      codecs.optional( codecs.bool, ( solverRequest, solverResponse ).tupled )

    val endId: Codec[V3EndId] =
      val process: Codec[V3EndId.Process]     = className[Recipe].xmap( V3EndId.Process( _ ), _.recipe )
      val input: Codec[V3EndId.Input]         = className[Item].xmap( V3EndId.Input( _ ), _.item )
      val requested: Codec[V3EndId.Requested] = className[Item].xmap( V3EndId.Requested( _ ), _.item )
      val byproduct: Codec[V3EndId.Byproduct] = className[Item].xmap( V3EndId.Byproduct( _ ), _.item )

      codecs
        .discriminated[V3EndId]
        .by( codecs.uint2 )
        .typecase( 0, process )
        .typecase( 1, input )
        .typecase( 2, requested )
        .typecase( 3, byproduct )

    val endSplits: Codec[Vector[( V3EndId, Vector[( ProcessSplitId, Double, Group )] )]] =
      codecs.vectorOfN(
        codecs.uint16,
        endId :: codecs.vectorOfN( codecs.int32, processSplitId :: codecs.double :: group )
      )

    val flows: Codec[V3Flows] =
      ( codecs.int32, processSplitId, endSplits, v4.itemFlowsMap ).imapN( V3Flows.apply )( Tuple.fromProductTyped )

    override def plan: Codec[P] =
      (
        planName,
        v4.modelVersionId,
        recipeOptions,
        resourceOptions,
        extractionOptions,
        logisticsOptions,
        powerOptions,
        requestSelection,
        solution,
        flows,
        productionUi
      ).imapN( P.apply )( Tuple.fromProductTyped )

    override def getPlanName( plan: P ): PlanName = plan.name

    override def getSummaryUpdated( summary: PlanSummary ): Instant = summary.updated

    override def toPlanSummary( planId: PlanId, plan: P, updated: Instant ): PlanSummary =
      PlanSummary( planId, plan.name, updated, plan.requestSelection.requestedAmounts.toVector )

  object v4 extends Codecs:
    type P = Plan
    type S = PlanSummary

    val P: Plan.type = Plan

    override val version: SchemaVersion = SchemaVersion( 4 )

    override def getPlanName( plan: Plan ): PlanName = plan.name

    override def getSummaryUpdated( summary: PlanSummary ): Instant = summary.updated

    override def toPlanSummary( planId: PlanId, plan: Plan, updated: Instant ): PlanSummary =
      PlanSummary( planId, plan.name, updated, plan.requestSelection.requestedAmounts.toVector )

    // do not use this if you ever want to add new enum members (except at the end)
    def enumOrdinal[A]( numCodec: Codec[Int], ev: CustomEnum[A] ): Codec[A] =
      numCodec.exmap(
        i => Attempt.fromOption( ev.cases.lift( i ), Err( s"$i not in [0;${ev.cases.length - 1}]" ) ),
        a => Attempt.successful( ev.indexOf( a ) )
      )

    def enumName[A]( ev: CustomEnum[A] ): Codec[A] =
      codecs.utf8_32.exmap(
        s => Attempt.fromEither( ev.withNameEither( s ).leftMap( Err( _ ) ) ),
        a => Attempt.successful( ev.keyOf( a ) )
      )

    def setOfN[A]( countCodec: Codec[Int], itemCodec: Codec[A] ): Codec[Set[A]] =
      codecs.vectorOfN( countCodec, itemCodec ).xmap( _.toSet, _.toVector )

    def sortedSetOfN[A: Ordering]( countCodec: Codec[Int], itemCodec: Codec[A] ): Codec[SortedSet[A]] =
      codecs.vectorOfN( countCodec, itemCodec ).xmap( _.to( SortedSet ), _.toVector )

    val path: Codec[Path] = codecs.utf8_32.xmap( Path( _ ), _.toString )

    val instant: Codec[Instant] = codecs.zlong.xmap( Instant.ofEpochMilli, _.toEpochMilli )

    def className[A]: Codec[ClassName[A]] = codecs.utf8_32.xmap( ClassName( _ ), _.name )

    val planId: Codec[PlanId] = codecs.int32.xmap( PlanId( _ ), _.id )

    override val planName: Codec[PlanName] = codecs.utf8_32.xmap( PlanName( _ ), _.name )

    override val planSummary: Codec[PlanSummary] =
      val requested: Codec[Vector[( ClassName[Item], Double )]] =
        codecs.vectorOfN( codecs.int16, className[Item] :: codecs.double )
      ( planId, planName, instant, requested ).imapN( PlanSummary.apply )( Tuple.fromProductTyped )

    val modelVersionId: Codec[ModelVersionId] =
      codecs.int16.xmap( ModelVersionId( _ ), _.id )

    val recipeOptions: Codec[RecipeOptions] =
      val allowedRecipes: Codec[Set[ClassName[Recipe.Manufacturing]]] =
        setOfN( codecs.int16, className[Recipe.Manufacturing] )
      ( codecs.bool, allowedRecipes ).imapN( RecipeOptions.apply )( Tuple.fromProductTyped )

    val extractorType: Codec[ExtractorType] = enumName( ExtractorType )

    val resourceDistrib: Codec[ResourceDistrib] =
      ( codecs.int16, codecs.int16, codecs.int16 ).imapN( ResourceDistrib.apply )( Tuple.fromProductTyped )

    val resourceOptions: Codec[ResourceOptions] =
      val vectorCodec: Codec[Vector[( ExtractorType, Vector[( ClassName[Item], ResourceDistrib )] )]] =
        codecs.vectorOfN(
          codecs.int8,
          extractorType :: codecs.vectorOfN( codecs.int16, className[Item] :: resourceDistrib )
        )
      vectorCodec.xmap(
        vector => ResourceOptions( vector.iterator.map { case ( ex, items ) => ( ex, items.toMap ) }.toMap ),
        _.resourceNodes.iterator.map { case ( ex, items ) => ( ex, items.toVector ) }.toVector
      )

    val resourceWeightSliders: Codec[Map[ClassName[Item], Int]] =
      codecs.vectorOfN( codecs.uint8, className[Item] :: codecs.int8 ).xmap( _.toMap, _.toVector )
    val extractionOptions: Codec[ExtractionOptions] =
      (
        className[Machine],
        enumName( ClockSpeedPreset.Extraction ),
        codecs.bool,
        setOfN( codecs.uint8, extractorType ),
        setOfN( codecs.uint16, className[Item] ),
        resourceWeightSliders
      ).imapN( ExtractionOptions.apply )( Tuple.fromProductTyped )

    val logisticsOptions: Codec[LogisticsOptions] =
      (
        codecs.optional( codecs.bool, className[Transport] ),
        codecs.optional( codecs.bool, className[Transport] ),
        sortedSetOfN( codecs.uint8, className[Transport] ),
        sortedSetOfN( codecs.uint8, className[Transport] )
      ).imapN( LogisticsOptions.apply )( Tuple.fromProductTyped )

    val powerOptions: Codec[PowerOptions] =
      (
        setOfN( codecs.uint8, className[Machine] ),
        codecs.uint8,
        enumOrdinal( codecs.uint4, ClockSpeedPreset )
      ).imapN( PowerOptions.apply )( Tuple.fromProductTyped )

    val requestSelection: Codec[RequestSelection] =
      codecs
        .vectorOfN( codecs.uint16, className[Item] :: codecs.double )
        .xmap( vector => RequestSelection( vector.to( SortedMap ) ), _.requestedAmounts.toVector )

    def countableDouble[A]( codecA: Codec[A] ): Codec[Countable[Double, A]] =
      ( codecA, codecs.double ).imapN( Countable.apply )( Tuple.fromProductTyped )

    val requestResource: Codec[SolverRequest.Resource] =
      (
        codecs.optional( codecs.bool, codecs.double ),
        codecs.double
      ).imapN( SolverRequest.Resource( _, _ ) )( Tuple.fromProductTyped )

    val solverRequest: Codec[SolverRequest] =
      (
        codecs.uint16.imap( ModelVersionId( _ ) )( _.id ),
        codecs.vectorOfN( codecs.uint16, countableDouble( className[Item] ) ),
        codecs.vectorOfN( codecs.uint16, className[Recipe.NonExtraction] ).imap( _.toSet )( _.toVector ),
        codecs.vectorOfN( codecs.uint8, className[Item] :: requestResource ).imap( _.toMap )( _.toVector ),
        className[Transport],
        className[Transport],
        codecs.uint8,
        enumOrdinal( codecs.uint4, ClockSpeedPreset )
      ).imapN( SolverRequest.apply )( Tuple.fromProductTyped )

    val clockSpeed: Codec[ClockSpeed] =
      codecs.double.xmap( ClockSpeed.ofPercent, _.percent )

    val boostedRecipe: Codec[BoostedRecipe[ClassName[Recipe.NonExtraction]]] =
      ( className[Recipe.NonExtraction], codecs.uint4, clockSpeed )
        .imapN( BoostedRecipe.apply )( Tuple.fromProductTyped )

    val solverResponse: Codec[SolverResponse.Solution] =
      (
        codecs.vectorOfN( codecs.uint16, countableDouble( className[Item] ) ),
        codecs.vectorOfN( codecs.int16, countableDouble( boostedRecipe ) )
      ).imapN( SolverResponse.Solution.apply )( Tuple.fromProductTyped )

    val solution: Codec[Option[( SolverRequest, SolverResponse.Solution )]] =
      codecs.optional( codecs.bool, ( solverRequest, solverResponse ).tupled )

    val processSplitId: Codec[ProcessSplitId] =
      codecs.int32.xmap( ProcessSplitId( _ ), _.id )

    val productionUi: Codec[ProductionUi] =
      (
        codecs.optional( codecs.bool, codecs.vectorOfN( codecs.int32, processSplitId ) ),
        codecs.vectorOfN( codecs.int32, processSplitId )
      ).imapN( ProductionUi.apply )( Tuple.fromProductTyped )

    val group: Codec[Group] =
      codecs.vectorOfN( codecs.uint8, codecs.uint16 ).xmap( Group( _ ), _.path )

    val flowEnd: Codec[FlowEnd] =
      codecs.bool.xmap( b => if ( b ) FlowEnd.Source else FlowEnd.Destination, _ == FlowEnd.Source )

    val endId: Codec[EndId] =
      val process: Codec[EndId.Process] =
        ( className[Recipe], codecs.uint4 ).imapN[EndId.Process]( EndId.Process( _, _ ) )( e => ( e.recipe, e.boost ) )
      val input: Codec[EndId.Input]         = className[Item].xmap( EndId.Input( _ ), _.item )
      val requested: Codec[EndId.Requested] = className[Item].xmap( EndId.Requested( _ ), _.item )
      val byproduct: Codec[EndId.Byproduct] = className[Item].xmap( EndId.Byproduct( _ ), _.item )

      codecs
        .discriminated[EndId]
        .by( codecs.uint2 )
        .typecase( 0, process )
        .typecase( 1, input )
        .typecase( 2, requested )
        .typecase( 3, byproduct )

    val itemFlows: Codec[ItemFlows] =
      (
        codecs.vectorOfN( codecs.int32, codecs.vectorOfN( codecs.int32, flowEnd :: processSplitId ) ),
        codecs.vectorOfN( codecs.int32, codecs.double :: codecs.uint16 :: codecs.uint16 )
      ).imapN( ItemFlows.apply )( Tuple.fromProductTyped )

    val endSplits: Codec[Vector[( EndId, Vector[( ProcessSplitId, Double, Group )] )]] =
      codecs.vectorOfN(
        codecs.uint16,
        endId :: codecs.vectorOfN( codecs.int32, processSplitId :: codecs.double :: group )
      )

    val itemFlowsMap: Codec[Map[ClassName[Item], ItemFlows]] =
      codecs
        .vectorOfN( codecs.int32, className[Item] :: itemFlows )
        .xmap( _.toMap, _.toVector )

    val flows: Codec[Flows] =
      ( codecs.int32, processSplitId, endSplits, itemFlowsMap ).imapN( Flows.apply )( Tuple.fromProductTyped )

    override val plan: Codec[Plan] =
      (
        planName,
        modelVersionId,
        recipeOptions,
        resourceOptions,
        extractionOptions,
        logisticsOptions,
        powerOptions,
        requestSelection,
        solution,
        flows,
        productionUi
      ).imapN( Plan.apply )( Tuple.fromProductTyped )

  abstract class Migration {
    type FromP
    type FromS

    type ToP
    type ToS

    def fromCodecs: Codecs.Aux[FromP, FromS]
    def toCodecs: Codecs.Aux[ToP, ToS]

    def upgradePlan( fromP: FromP ): ToP

    def describe: String       = s"${fromCodecs.version} -> ${toCodecs.version}"
    def version: SchemaVersion = toCodecs.version
  }

  object Migration:
    type Aux[P0, S0, P1, S1] =
      Migration { type FromP = P0; type FromS = S0; type ToP = P1; type ToS = S1 }

    def apply[P0, S0, P1, S1](
        fromCodecs0: Codecs.Aux[P0, S0],
        toCodecs0: Codecs.Aux[P1, S1]
    )(
        upgradePlan0: P0 => P1
    ): Migration.Aux[P0, S0, P1, S1] =
      new Migration {
        type FromP = P0
        type FromS = S0
        type ToP   = P1
        type ToS   = S1

        override val fromCodecs: Codecs.Aux[P0, S0] = fromCodecs0
        override val toCodecs: Codecs.Aux[P1, S1]   = toCodecs0

        override def upgradePlan( fromP: P0 ): P1 = upgradePlan0( fromP )
      }

  private def upgradePlanToV2( plan: v1.P ): v2.P =
    v2.P(
      plan.name,
      plan.recipeOptions,
      plan.resourceOptions,
      plan.extractionOptions,
      plan.logisticsOptions,
      plan.powerOptions,
      plan.requestSelection,
      v3.V3Flows(
        plan.flows.prodHash,
        plan.flows.nextId,
        plan.flows.endSplits,
        plan.flows.itemFlows
          .fmap: itemTransports =>
            ItemFlows( itemTransports, Vector.empty )
      ),
      plan.productionUi
    )

  private def upgradePlanToV3( plan: v2.P ): v3.P =
    v3.P(
      plan.name,
      ModelVersionId( 7 ),
      plan.recipeOptions,
      plan.resourceOptions,
      plan.extractionOptions,
      plan.logisticsOptions,
      plan.powerOptions,
      plan.requestSelection,
      none,
      plan.flows,
      plan.productionUi
    )

  private def upgradePlanToV4( plan: v3.P ): v4.P =
    val extractionOptions: ExtractionOptions =
      ExtractionOptions(
        plan.extractionOptions.minerClass,
        plan.extractionOptions.clockSpeed,
        true,
        plan.extractionOptions.extractors,
        plan.extractionOptions.preferFracking,
        plan.extractionOptions.resourceWeightSliders
      )

    val powerOptions: PowerOptions =
      PowerOptions(
        plan.powerOptions.allowedGenerators,
        0,
        ClockSpeedPreset.`100%`
      )

    val solution: Option[( SolverRequest, SolverResponse.Solution )] = plan.solution.map:
      case ( request, response ) =>
        (
          SolverRequest(
            request.modelVersion,
            request.requested,
            request.recipeSelection,
            request.resources,
            plan.logisticsOptions.singleBelt.getOrElse( plan.logisticsOptions.allBelts.last ),
            plan.logisticsOptions.singlePipeline.getOrElse( plan.logisticsOptions.allPipelines.last ),
            0,
            ClockSpeedPreset.`100%`
          ),
          SolverResponse.Solution(
            response.inputs,
            response.recipes.map( _.map( BoostedRecipe( _, 0, ClockSpeedPreset.`100%`.value ) ) )
          )
        )

    def upgradeFlowEndSplit[A]( es: ( v3.V3EndId, A ) ): ( EndId, A ) =
      val endId =
        es._1 match
          case v3.V3EndId.Process( recipe ) => EndId.Process( recipe, 0 )
          case v3.V3EndId.Input( item )     => EndId.Input( item )
          case v3.V3EndId.Requested( item ) => EndId.Requested( item )
          case v3.V3EndId.Byproduct( item ) => EndId.Byproduct( item )
      ( endId, es._2 )

    val flows: Flows =
      Flows(
        plan.flows.prodHash,
        plan.flows.nextId,
        plan.flows.endSplits.map( upgradeFlowEndSplit ),
        plan.flows.itemFlows
      )

    v4.P(
      plan.name,
      plan.modelVersionId,
      plan.recipeOptions,
      plan.resourceOptions,
      extractionOptions,
      plan.logisticsOptions,
      powerOptions,
      plan.requestSelection,
      solution,
      flows,
      plan.productionUi
    )

  val migrations: Vector[Migration] =
    Vector(
      Migration( v1, v2 )( upgradePlanToV2 ),
      Migration( v2, v3 )( upgradePlanToV3 ),
      Migration( v3, v4 )( upgradePlanToV4 )
    )
