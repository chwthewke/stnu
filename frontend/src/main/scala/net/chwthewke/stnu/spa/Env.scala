package net.chwthewke.stnu
package spa

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
