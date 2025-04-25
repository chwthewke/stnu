package net.chwthewke.stnu
package model

import alleycats.std.iterable.*
import cats.Show
import cats.syntax.all.*
import scala.collection.immutable.SortedMap

case class Model(
    version: ModelVersion,
    items: SortedMap[ClassName, Item],
    extractedItems: Vector[Item],
    manufacturingRecipes: Vector[Recipe.Prod],
    powerRecipes: Vector[Recipe.PowerGen],
    extractionRecipes: Vector[( Item, ResourcePurity, Recipe.Prod )],
    machines: SortedMap[ClassName, Machine],
    conveyorBelts: Vector[Transport],
    pipelines: Vector[Transport],
    defaultResourceOptions: ResourceOptions
)

object Model:

  given Show[Model] = Show.show: model =>
    show"""Manufacturing Recipes
          |${model.manufacturingRecipes.map( _.show ).intercalate( "\n" )}
          |
          |Items
          |${model.items.values.map( _.toString ).intercalate( "\n" )}
          |
          |Extracted Items ${model.extractedItems.map( _.displayName ).intercalate( ", " )}
          |
          |Extraction Recipes
          |${model.extractionRecipes.map( _._3 ).map( _.show ).intercalate( "\n" )}
          |
          |Resource nodes
          |${model.defaultResourceOptions.show.linesIterator.map( "  " + _ ).toSeq.mkString_( "\n" )}
          |""".stripMargin
