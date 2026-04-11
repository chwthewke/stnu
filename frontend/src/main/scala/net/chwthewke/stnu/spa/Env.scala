package net.chwthewke.stnu
package spa

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptyVector
import cats.syntax.all.*
import org.http4s.Uri
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag

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
  // Model
  ///////////////

  def getItem( className: ClassName[Item] ): Option[Item] =
    game.items.get( className )

  def getRecipe[R <: Recipe]( className: ClassName[R] )( using R: ClassTag[R] ): Option[R] =
    game.recipes.get( className ).flatMap( R.unapply( _ ) )

  ///////////////
  // Recipe presets
  ///////////////

  // TODO this is pretty uncomfortable
  lazy val withoutFicsmas: Env =
    copy( game = ModelConsistency( game, ( _, ex ) => ex != ExtractorType.FicsmasTree ).getOrElse( game ) )

  def manufacturingRecipesWhere( pred: Recipe.Manufacturing => Boolean ): Set[ClassName[Recipe.Manufacturing]] =
    game.manufacturingRecipes
      .mapFilter( recipe => Option.when( pred( recipe ) )( recipe.className ) )
      .toSet

  lazy val nonExtractionRecipes: Vector[Recipe.NonExtraction] = game.manufacturingRecipes ++ game.powerRecipes

  lazy val sortedRecipes: Vector[Recipe.NonExtraction] =
    val byTier: SortedMap[Tier, Vector[Recipe.NonExtraction]] = nonExtractionRecipes.foldMap: recipe =>
      SortedMap( recipe.category.tier -> Vector( recipe ) )

    @tailrec
    def sortTier[R <: Recipe: Ordering](
        productAcc: Set[ClassName[Item]],
        acc: Vector[R],
        toSort: Vector[R]
    ): ( Set[ClassName[Item]], Vector[R] ) =
      if ( toSort.isEmpty ) ( productAcc, acc )
      else
        val ( feasible, infeasible ) =
          toSort.partition: recipe =>
            recipe.ingredients.forall( item => productAcc.contains( item.item.className ) )
        if ( feasible.isEmpty ) ( productAcc, acc ++ infeasible.sorted )
        else
          val feasibleProducts: Set[ClassName[Item]] =
            feasible.foldMap( r => r.productsList.map( _.item.className ).toSet )
          sortTier( productAcc ++ feasibleProducts, acc ++ feasible.sorted, infeasible )

    @tailrec
    def sort[R <: Recipe: Ordering](
        productAcc: Set[ClassName[Item]],
        acc: Vector[R],
        toSort: SortedMap[Tier, Vector[R]]
    ): Vector[R] =
      if ( toSort.isEmpty ) acc
      else
        val ( nextProductAcc, sortedTier ) = sortTier( productAcc, Vector.empty, toSort.head._2 )
        sort( nextProductAcc, acc ++ sortedTier, toSort.tail )

    sort( game.extractedItems.map( _.className ).toSet, Vector.empty, byTier )

  lazy val recipeOrdering: Ordering[Recipe] =
    val m: Map[ClassName[Recipe], Int] =
      sortedRecipes.zipWithIndex
        .map:
          case ( r, ix ) => ( r.className, ix )
        .toMap

    Ordering.by( recipe => ( m.get( recipe.className ), recipe ) )

  lazy val itemOrder: Ordering[Item] =
    val m: Map[ClassName[Item], Int] =
      ( game.extractedItems.sortBy( _.displayName ).map( _.className ) ++
        game.items.values.toVector
          .sortBy: item =>
            ( item.tier, item.displayName )
          .map( _.className ) ).distinct.zipWithIndex.toMap

    Ordering.by( item => m.get( item.className ) )

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
      .mapFilter( getItem )
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

  /////////////////////
  // Power generators
  /////////////////////

  lazy val powerGenerators: Vector[Machine] =
    game.machines.values
      .filter( _.machineType.isPowerGenerator )
      .toVector
      .sortBy: machine =>
        game.recipes.values
          .filter( _.producedIn == machine )
          .map( _.category.tier )
          .toVector
          .minimumOption
