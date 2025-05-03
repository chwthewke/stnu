package net.chwthewke.stnu
package service
package solver

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import munit.AnyFixture
import munit.CatsEffectSuite
import munit.catseffect.IOFixture

import data.Countable
import model.Item
import model.Model
import protocol.solver.SolverRequest

class SolverServiceTests extends CatsEffectSuite:
  private val loadModels: IO[Map[ModelVersionId, Model]] =
    for
      modelIndex <- assets.loadModelIndex[IO]
      models: Vector[Model] <- modelIndex.versions.toVector.traverse: version =>
                                 assets.loadModel[IO]( version )
    yield models.fproductLeft( _.version.version ).toMap

  val modelsFixture: IOFixture[SolverService[IO]] =
    ResourceSuiteLocalFixture(
      "models",
      Resource.eval( loadModels ).map( models => new SolverService[IO]( models, new ConstraintSolver[IO] ) )
    )

  override def munitFixtures: Seq[AnyFixture[?]] = List( modelsFixture )

  def checkCanProduce( model: Model, item: Item )( using loc: munit.Location ): Unit =
    test( s"Can produce ${item.displayName} in ${model.version.name}" ):
      val requested       = Vector( Countable( item.className, 1d ) )
      val recipeSelection = model.manufacturingRecipes.map( _.className ) ++ model.powerRecipes.map( _.className )
      val resources =
        model.extractedItems
          .map: item =>
            ( item.className, SolverRequest.Resource( 1e9d, 1d ) )
          .toMap

      val request = SolverRequest( model.version.version, requested, recipeSelection, resources )
      val solver  = modelsFixture()
      solver
        .solveEither( request )
        .value
        .map( _.left.toOption )
        .assertEquals( None )

  def allProducts( model: Model ): Vector[Item] =
    (
      model.recipes.values.flatMap( recipe => recipe.productsList.map( _.item ) ).toVector ++
        model.extractedItems
    ).distinct

  loadModels
    .map( models =>
      for
        ( version, model ) <- models
        item               <- allProducts( model.masked )
      do checkCanProduce( model, item )
    )
    .unsafeRunSync()
