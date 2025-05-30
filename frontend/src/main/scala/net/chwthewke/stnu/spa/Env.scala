package net.chwthewke.stnu
package spa

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptyVector
import cats.syntax.all.*
import org.http4s.Uri
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import model.ExtractorType
import model.Item
import model.Machine
import model.Model
import model.ModelConsistency
import model.Recipe
import model.Tier
import model.Transport

// NOTE general purpose "environment" (things that change only when switching game versions),
//  containing generally required content-related information
case class Env(
    game: Model,
    itemIcons: Map[ClassName[Item], Uri],
    machineIcons: Map[ClassName[Machine], Uri],
    transportIcons: Map[ClassName[Transport], Uri]
):
  ///////////////
  // Recipe presets
  ///////////////

  lazy val withoutFicsmas: Env =
    copy( game = ModelConsistency( game, ( _, ex ) => ex != ExtractorType.FicsmasTree ).getOrElse( game ) )

  def manufacturingRecipesWhere( pred: Recipe.Manufacturing => Boolean ): Set[ClassName[Recipe.Manufacturing]] =
    game.manufacturingRecipes
      .mapFilter( recipe => Option.when( pred( recipe ) )( recipe.className ) )
      .toSet

  lazy val nonExtractionRecipes: Vector[Recipe.NonExtraction] =
    ( game.manufacturingRecipes ++ game.powerRecipes )

  def sortRecipes: Vector[Recipe.NonExtraction] =
    val byTier: SortedMap[Tier, Vector[Recipe.NonExtraction]] = nonExtractionRecipes.foldMap: recipe =>
      SortedMap( recipe.category.tier -> Vector( recipe ) )

    @tailrec
    def sortTier[R <: Recipe](
        productAcc: Set[ClassName[Item]],
        acc: Vector[R],
        toSort: Vector[R]
    ): ( Set[ClassName[Item]], Vector[R] ) =
      if ( toSort.isEmpty ) ( productAcc, acc )
      else
        val ( feasible, infeasible ) =
          toSort.partition: recipe =>
            recipe.ingredients.forall( item => productAcc.contains( item.item.className ) )
        if ( feasible.isEmpty )
          // FIXME println
          println(
            infeasible
              .map( _.displayName )
              .mkString_(
                show"[WARN] infeasible in tier ${infeasible.head.category.tier}: ",
                ", ",
                "."
              )
          )
          ( productAcc, acc ++ infeasible.sortBy( _.displayName ) )
        else
          val feasibleProducts: Set[ClassName[Item]] =
            feasible.foldMap( r => r.productsList.map( _.item.className ).toSet )
          sortTier( productAcc ++ feasibleProducts, acc ++ feasible.sortBy( _.displayName ), infeasible )

    @tailrec
    def sort[R <: Recipe](
        productAcc: Set[ClassName[Item]],
        acc: Vector[R],
        toSort: SortedMap[Tier, Vector[R]]
    ): Vector[R] =
      if ( toSort.isEmpty ) acc
      else
        val ( nextProductAcc, sortedTier ) = sortTier( productAcc, Vector.empty, toSort.head._2 )
        sort( nextProductAcc, acc ++ sortedTier, toSort.tail )

    sort( game.extractedItems.map( _.className ).toSet, Vector.empty, byTier )

  ///////////////
  // Extraction
  ///////////////

  lazy val itemsExtractibleByFrackingAndOtherMethod: List[Item] =
    val extractorTypesByItem: Map[ClassName[Item], Set[ExtractorType]] =
      game.defaultResourceOptions.resourceNodes
        .fmap( _.keys )
        .toVector
        .foldMap:
          case ( extractor, items ) =>
            items.toVector.foldMap( item => Map( item -> Set( extractor ) ) )
    extractorTypesByItem
      .filter:
        case ( _, extractors ) => extractors.size > 1 && extractors.contains( ExtractorType.Fracking )
      .keys
      .toList
      .mapFilter( game.items.get )
      .sortBy( _.displayName )

  lazy val miners: List[Machine] =
    game.machines.values
      .filter: m =>
        m.machineType.extractor.contains( ExtractorType.Miner )
      .toList
      .sortBy( _.powerConsumption )

  lazy val machinesByExtractorType: SortedMap[ExtractorType, Machine] =
    game.machines.values.toVector
      .foldMap: machine =>
        machine.machineType.extractor.foldMap( extractor => SortedMap( extractor -> NonEmptyVector.one( machine ) ) )
      .fmap( _.maximumBy( _.powerConsumption ) )

  ///////////////
  // Logistics
  ///////////////

  lazy val conveyorBelts: NonEmptyList[Transport] = game.conveyorBelts.sortBy( _.perMinute ).toNonEmptyList

  lazy val defaultPipelines: NonEmptyList[Transport] = game.pipelines
    .reduceMap: pipeline =>
      NonEmptyMap.one( pipeline.perMinute, NonEmptyVector.one( pipeline ) )
    .toNel
    .map( _._2.minimumBy( _.displayName.length ) )
    .sortBy( _.perMinute )
