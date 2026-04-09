package net.chwthewke.stnu
package protocol
package solver

import cats.syntax.all.*
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

import data.Countable
import model.ClockSpeed
import model.ClockSpeedPreset
import model.ExtractorType
import model.Feasible
import model.Item
import model.Machine
import model.Model
import model.Recipe
import model.ResourceWeights
import model.Tier
import model.Transport
import service.solver.ConstraintSolver
import service.solver.SolverService

trait SolutionGenerators:
  type Recipes   = Set[ClassName[Recipe.NonExtraction]]
  type Requested = Vector[Countable[Double, ClassName[Item]]]

  val defaultTier: Gen[Tier] = Gen.choose( 1, 5 ).map( n => Tier( ( n * 2 ).min( 9 ) ) )

  def defaultMiner( model: Model )( tierGen: Gen[Tier] = defaultTier ): Gen[ClassName[Machine]] =
    tierGen.flatMap: tier =>
      model.machines.values
        .filter( m => m.machineType.extractor.contains( ExtractorType.Miner ) && m.tier <= tier )
        .toVector
        .toNev
        .map( _.maximumBy( _.tier ) )
        .fold( Gen.fail )( m => Gen.const( m.className ) )

  def defaultExtractors( model: Model )( tierGen: Gen[Tier] = defaultTier ): Gen[Set[ExtractorType]] =
    tierGen.map: tier =>
      model.machines.values.toVector
        .mapFilter( m => m.machineType.extractor.filter( _ != ExtractorType.Miner && m.tier <= tier ) )
        .toSet

  def defaultConveyorBelt( model: Model ): Tier => Gen[Transport] = tier =>
    if ( tier.value >= 9 )
      model.conveyorBelts.toVector( 5 )
    else if ( tier.value >= 7 )
      model.conveyorBelts.toVector( 4 )
    else if ( tier.value >= 3 )
      model.conveyorBelts.toVector( 3 )
    else if ( tier.value >= 1 )
      model.conveyorBelts.toVector( 1 )
    else
      model.conveyorBelts.toVector( 0 )

  def defaultPipeline( model: Model ): Tier => Gen[Transport] = tier =>
    if ( tier.value >= 5 )
      model.pipelines.toVector( 1 )
    else
      model.pipelines.toVector( 0 )

  def defaultMaxProductionBoost( tier: Tier ): Gen[Int] =
    if ( tier.value >= 5 ) Gen.oneOf( Gen.choose( 1, 50 ), Gen.const( 0 ) )
    else Gen.const( 0 )

  def recipeSelection( model: Model )( tierGen: Gen[Tier] = defaultTier ): Gen[Set[ClassName[Recipe.NonExtraction]]] =
    (
      tierGen,
      Gen.oneOf( false, true ),
      Gen.oneOf( false, true )
    ).mapN: ( tier, alt, matConv ) =>
      ( model.manufacturingRecipes ++ model.powerRecipes )
        .filter: recipe =>
          recipe.category.tier <= tier
            && ( alt || !recipe.isAlternate )
            && ( matConv || !recipe.isMatterConversion )
        .map( _.className )
        .toSet

  def requestSelection( model: Model )(
      recipeSelectionGen: Gen[Set[ClassName[Recipe.NonExtraction]]] = recipeSelection( model )(),
      sizeGen: Int => Gen[Int] = ( n: Int ) => Gen.choose( 1, n )
  ): Gen[Vector[Countable[Double, ClassName[Item]]]] =
    for
      recipes <- recipeSelectionGen
      feasible: Vector[ClassName[Item]] =
        Feasible(
          model,
          allowRecipe = ( recipe: Recipe.NonExtraction ) => recipes.contains_( recipe.className ),
          forcedItems = Set.empty
        )._1.diff( model.extractedItems.map( _.className ).toSet ).toVector
      size      <- sizeGen( feasible.length )
      picked    <- Gen.pick( size.min( feasible.length ), feasible )
      requested <-
        picked.toVector.traverseFilter: item =>
          model.items
            .get( item )
            .map( _.tier )
            .traverse: tier =>
              Gen.choose( 1, 10 - tier.value ).map( amt => Countable( item, amt.toDouble ) )
    yield requested

  def solverRequest( model: Model )(
      tierGen: Gen[Tier] = defaultTier,
      recipeSelectionGen: Tier => Gen[Recipes] = tier => retry( recipeSelection( model )( tier ), 13 ),
      requestSelectionGen: Recipes => Gen[Requested] = requestSelection( model )( _ ),
      minerSelection: Tier => Gen[ClassName[Machine]] = tier => retry( defaultMiner( model )( tier ), 15 ),
      extractorSelection: Tier => Gen[Set[ExtractorType]] = tier => retry( defaultExtractors( model )( tier ), 17 ),
      bestConveyorBeltGen: Tier => Gen[Transport] = tier => retry( defaultConveyorBelt( model )( tier ), 19 ),
      bestPipelineGen: Tier => Gen[Transport] = tier => retry( defaultPipeline( model )( tier ), 21 ),
      maxProductionBoostGen: Tier => Gen[Int] = defaultMaxProductionBoost,
      manufacturingClockSpeedGen: Gen[ClockSpeedPreset] = Gen.oneOf( ClockSpeedPreset.cases )
  ): Gen[SolverRequest] =
    for
      clockSpeed              <- Gen.oneOf( ClockSpeedPreset.cases )
      tier                    <- tierGen
      minerClass              <- minerSelection( tier )
      extractors              <- extractorSelection( tier )
      recipes                 <- recipeSelectionGen( tier )
      requested               <- requestSelectionGen( recipes )
      bestConveyorBelt        <- bestConveyorBeltGen( tier )
      bestPipeline            <- bestPipelineGen( tier )
      maxProductionBoost      <- maxProductionBoostGen( tier )
      manufacturingClockSpeed <- manufacturingClockSpeedGen
    yield SolverRequest(
      model.version.version,
      requested,
      recipes,
      model
        .resources(
          minerClass,
          clockSpeed,
          extractors,
          model.defaultResourceOptions.resourceNodes,
          ResourceWeights( model.extractedItems.map( item => ( item.className, 0 ) ).toMap )
        )
        .fmap { case ( cap, cost ) => SolverRequest.Resource( cap, cost ) },
      bestConveyorBelt.className,
      bestPipeline.className,
      maxProductionBoost,
      manufacturingClockSpeed
    )

  def solverRequestAndResponse( model: Model )(
      requestGen: Gen[SolverRequest] = solverRequest( model )()
  ): Gen[( SolverRequest, SolverResponse.Solution )] =
    requestGen.flatMap: request =>
      val requested                             = request.requested.mapFilter( _.traverse( model.items.get ) )
      val recipes: Vector[Recipe.NonExtraction] =
        ( model.manufacturingRecipes ++ model.powerRecipes ).filter( r => request.recipeSelection( r.className ) )
      val inputs                      = request.resources
      val bestConveyorBelt: Transport =
        model.conveyorBelts.find( _.className == request.bestConveyorBelt ).getOrElse( model.conveyorBelts.last )
      val bestPipeline: Transport =
        model.pipelines.find( _.className == request.bestPipeline ).getOrElse( model.pipelines.last )
      val recipesWithClockSpeed: Vector[( Recipe.NonExtraction, ClockSpeed )] =
        recipes.fproduct( SolverService.maxClockSpeed( bestConveyorBelt, bestPipeline ) )

      ConstraintSolver
        .solve(
          requested,
          recipesWithClockSpeed,
          inputs,
          request.maxProductionBoost,
          request.manufacturingClockSpeed
        )
        .fold( _ => Gen.fail, Gen.const )
        .tupleLeft( request )

object SolutionGenerators extends SolutionGenerators
