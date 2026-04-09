package net.chwthewke.stnu
package persistence

import cats.data.NonEmptyVector
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import model.ClockSpeedPreset
import model.ExtractorType
import model.Model
import model.ResourceDistrib
import model.ResourceWeights
import protocol.persistence.ExtractionOptions
import protocol.persistence.Flows
import protocol.persistence.LogisticsOptions
import protocol.persistence.Plan
import protocol.persistence.PlanName
import protocol.persistence.PowerOptions
import protocol.persistence.ProcessSplitId
import protocol.persistence.ProductionUi
import protocol.persistence.RecipeOptions
import protocol.persistence.RequestSelection
import protocol.persistence.ResourceOptions

object PlanGenerators:
  def pick[A]( src: Seq[A] ): Gen[collection.Seq[A]] = Gen.choose( 0, src.length ).flatMap( Gen.pick( _, src ) )

  def shuffle[A]( seq: IndexedSeq[A] ): Gen[List[A]] =
    ( List.empty[A], seq )
      .tailRecM: ( acc: List[A], rest: IndexedSeq[A] ) =>
        if ( rest.isEmpty ) Gen.const( Right( acc ) )
        else
          Gen
            .choose( 0, rest.length - 1 )
            .map( ix => Left( ( rest( ix ) :: acc, rest.patch( ix, Nil, 1 ) ) ) )

  def recipeOptions( model: Model ): Gen[RecipeOptions] =
    for
      hideFicsmas <- Gen.oneOf( false, true )
      recipes     <- pick( model.manufacturingRecipes )
    yield RecipeOptions( hideFicsmas, recipes.map( _.className ).toSet )

  def resourceOptions( model: Model ): Gen[ResourceOptions] =
    def subDistrib( distrib: ResourceDistrib ): Gen[ResourceDistrib] =
      (
        Gen.choose( 0, distrib.impureNodes ),
        Gen.choose( 0, distrib.normalNodes ),
        Gen.choose( 0, distrib.pureNodes )
      ).mapN( ResourceDistrib( _, _, _ ) )

    model.defaultResourceOptions.resourceNodes
      .to( SortedMap )
      .traverse: byItem =>
        byItem
          .to( SortedMap )
          .traverse( subDistrib )
      .map( ResourceOptions( _ ) )

  def extractionOptions( model: Model ): Gen[ExtractionOptions] =
    for
      miner <-
        Gen.oneOf( model.machines.values.filter( _.machineType.extractor.contains_( ExtractorType.Miner ) ).toSeq )
      clockSpeed                       <- Gen.oneOf( ClockSpeedPreset.Extraction.cases )
      excludeWaterPumpFromOverclocking <- Gen.oneOf( false, true )
      extractors                       <- pick( ExtractorType.cases )
      preferFracking                   <- pick( model.extractedItems )
      resourceWeights                  <-
        model.extractedItems.traverse( Gen.choose( -ResourceWeights.range, ResourceWeights.range ).tupleLeft )
    yield ExtractionOptions(
      miner.className,
      clockSpeed,
      excludeWaterPumpFromOverclocking,
      extractors.toSet,
      preferFracking.map( _.className ).toSet,
      resourceWeights.map { case ( item, weight ) => ( item.className, weight ) }.toMap
    )

  def logisticsOptions( model: Model ): Gen[LogisticsOptions] =
    def pickInOrder[A]( src: NonEmptyVector[A] ): Gen[NonEmptyVector[A]] =
      src.toVector
        .traverseFilter: elt =>
          Gen.oneOf( false, true ).map( b => Option.when( b )( elt ) )
        .suchThat( _.nonEmpty )
        .map( NonEmptyVector.fromVectorUnsafe )

    for
      belts                          <- pickInOrder( model.conveyorBelts.map( _.className ) )
      pipelines                      <- pickInOrder( model.pipelines.map( _.className ) )
      ( singleBelt, singlePipeline ) <-
        Gen.oneOf(
          ( belts.last.some, pipelines.last.some ),
          ( none, none )
        )
    yield LogisticsOptions(
      singleBelt,
      singlePipeline,
      belts.toVector.to( SortedSet ),
      pipelines.toVector.to( SortedSet )
    )

  def powerOptions( model: Model ): Gen[PowerOptions] =
    pick( model.machines.values.filter( _.machineType.isPowerGenerator ).toSeq ).map: generators =>
      PowerOptions( generators.map( _.className ).toSet, 0, ClockSpeedPreset.`100%` )

  def requestSelection( model: Model ): Gen[RequestSelection] =
    for
      items   <- pick( model.items.values.toSeq )
      amounts <- items.toVector.traverse( item =>
                   Gen.choose( 0.0d, math.log( 500.0d ) ).map( math.exp ).tupleLeft( item.className )
                 )
    yield RequestSelection( amounts.to( SortedMap ) )

  // we know it's just going into a JSON blob, let's not overdo it
  def flows( model: Model ): Gen[Flows] =
    (
      Arbitrary.arbitrary[Int],
      Gen.choose( 1, 1000 ).map( ProcessSplitId( _ ) )
    ).mapN( Flows( _, _, Vector.empty, Map.empty ) )

  def productionUi( model: Model )( flows: Flows ): Gen[ProductionUi] =
    val ids: IndexedSeq[ProcessSplitId] = ( 0 until flows.nextId.id ).map( ProcessSplitId( _ ) )
    (
      Gen.option( shuffle( ids ).map( _.toVector ) ),
      pick( ids )
    ).mapN: ( order, complete ) =>
      ProductionUi( order, complete.toVector )

  def plan( model: Model )(
      nameGen: Gen[PlanName] = Gen.alphaNumStr.map( PlanName( _ ) ),
      recipeOptionsGen: Gen[RecipeOptions] = recipeOptions( model ),
      resourceOptionsGen: Gen[ResourceOptions] = resourceOptions( model ),
      extractionOptionsGen: Gen[ExtractionOptions] = extractionOptions( model ),
      logisticsOptionsGen: Gen[LogisticsOptions] = logisticsOptions( model ),
      powerOptionsGen: Gen[PowerOptions] = powerOptions( model ),
      requestSelectionGen: Gen[RequestSelection] = requestSelection( model ),
      flowsGen: Gen[Flows] = flows( model ),
      productionUiGen: Flows => Gen[ProductionUi] = productionUi( model )
  ): Gen[Plan] =
    for
      name              <- nameGen
      recipeOptions     <- recipeOptionsGen
      resourceOptions   <- resourceOptionsGen
      extractionOptions <- extractionOptionsGen
      logisticsOptions  <- logisticsOptionsGen
      powerOptions      <- powerOptionsGen
      requestSelection  <- requestSelectionGen
      flows             <- flowsGen
      productionUi      <- productionUiGen( flows )
    yield Plan(
      name,
      model.version.version,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requestSelection,
      none,
      flows,
      productionUi
    )
