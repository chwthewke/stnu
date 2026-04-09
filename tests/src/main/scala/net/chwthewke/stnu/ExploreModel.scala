package net.chwthewke.stnu

import cats.syntax.all.*
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

import model.*

object ExploreModel extends IOApp:
  def loadModel( version: DataVersionStorage = DataVersionStorage.Release1_1 ): IO[Model] =
    assets.loadModel[IO]( version.modelVersion )

  def showIngredientAndProductCount( model: Model ): String =
    val counts: Map[ClassName[Recipe], ( Recipe, Int, Int )] =
      model.recipes.fmap: recipe =>
        ( recipe, recipe.ingredients.length, recipe.products.size.toInt )

    val lines =
      counts
        .map:
          case ( recipe, ( _, ings, prods ) ) =>
            s"${ings + prods} ($ings, $prods) $recipe"
        .mkString( "\n" )
    s"""$lines
       |MIN=${counts.fmap( t => t._2 + t._3 ).values.min}""".stripMargin

  def showShort( model: Model ): String =
    s"""MODEL version ${model.version}
       |  ${model.items.size} items
       |  ${model.recipes.size} recipes
       |  ${model.machines.size} machines
       |  ${model.conveyorBelts.length} conveyor belts
       |  ${model.pipelines.length} pipelines
       |""".stripMargin

  override def run( args: List[String] ): IO[ExitCode] =
    loadModel()
      .map( showShort )
      .flatMap( IO.println )
      .as( ExitCode.Success )
