package net.chwthewke.stnu
package protocol
package solver

import cats.data.NonEmptyVector
import cats.syntax.all.*
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

import data.Countable
import model.ClockSpeedPreset
import model.ExtractorType
import model.Feasible
import model.Item
import model.Machine
import model.Model
import model.Recipe
import model.Tier
import service.solver.ConstraintSolver

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
          allowRecipe = ( recipe: Recipe.NonExtraction ) => recipes.contains_( recipe.className )
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
      recipeSelectionGen: Tier => Gen[Recipes] = recipeSelection( model )( _ ),
      requestSelectionGen: Recipes => Gen[Requested] = requestSelection( model )( _ ),
      minerSelection: Tier => Gen[ClassName[Machine]] = defaultMiner( model ),
      extractorSelection: Tier => Gen[Set[ExtractorType]] = defaultExtractors( model )
  ): Gen[SolverRequest] =
    for
      clockSpeed <- Gen.oneOf( ClockSpeedPreset.cases )
      tier       <- tierGen
      minerClass <- minerSelection( tier )
      extractors <- extractorSelection( tier )
      recipes    <- recipeSelectionGen( tier )
      requested  <- requestSelectionGen( recipes )
    yield SolverRequest(
      model.version.version,
      requested,
      recipes,
      model
        .resourceCaps(
          minerClass,
          clockSpeed,
          extractors,
          model.defaultResourceOptions.resourceNodes
        )
        .fmap( cap => SolverRequest.Resource( cap = cap, weight = 1d ) )
    )

  def solverRequestAndResponse( model: Model )(
      requestGen: Gen[SolverRequest] = solverRequest( model )()
  ): Gen[( SolverRequest, SolverResponse.Ok )] =
    requestGen.flatMap: request =>
      val requested = request.requested.mapFilter( _.traverse( model.items.get ) )
      val recipes   =
        ( model.manufacturingRecipes ++ model.powerRecipes ).filter( r => request.recipeSelection( r.className ) )
      val inputs = request.resources
      ConstraintSolver
        .solve( requested, recipes, inputs )
        .fold( _ => Gen.asciiStr /* advance seed */ >> Gen.fail, Gen.const )
        .tupleLeft( request )

object SolutionGenerators extends SolutionGenerators
