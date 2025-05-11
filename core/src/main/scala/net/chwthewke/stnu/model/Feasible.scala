package net.chwthewke.stnu
package model

import cats.syntax.all.*
import scala.annotation.tailrec

object Feasible:
  private val True: Any => Boolean = Function.const( true )

  def apply(
      model: Model,
      allowExtractedItem: Item => Boolean = True,
      allowOtherItem: Item => Boolean = True,
      allowRecipe: Recipe.NonExtraction => Boolean = True,
      initialItems: Set[ClassName[Item]] = Set.empty,
      initialRecipes: Set[ClassName[Recipe.NonExtraction]] = Set.empty
  ): ( Set[ClassName[Item]], Set[ClassName[Recipe.NonExtraction]] ) =
    val allowedExtractedItems: Set[ClassName[Item]] =
      model.extractedItems.filter( allowExtractedItem( _ ) ).map( _.className ).toSet

    val allowedRecipes: Vector[Recipe.NonExtraction] =
      ( model.manufacturingRecipes ++ model.powerRecipes ).filter( recipe => allowRecipe( recipe ) )

    @tailrec
    def loop(
        itemsAcc: Set[ClassName[Item]],
        recipesAcc: Set[ClassName[Recipe.NonExtraction]]
    ): ( Set[ClassName[Item]], Set[ClassName[Recipe.NonExtraction]] ) =
      val moreRecipes: Vector[Recipe.NonExtraction] =
        allowedRecipes
          .filter: recipe =>
            !recipesAcc.contains_( recipe.className )
              && recipe.ingredients.forall( ci => itemsAcc.contains_( ci.item.className ) )
      val moreItems: Set[ClassName[Item]] =
        moreRecipes.iterator
          .flatMap( recipe => recipe.productsList.map( _.item ).iterator )
          .distinct
          .filter( item => !itemsAcc.contains_( item.className ) && allowOtherItem( item ) )
          .map( _.className )
          .toSet

      if ( moreItems.isEmpty ) ( itemsAcc, recipesAcc )
      else loop( itemsAcc ++ moreItems, recipesAcc ++ moreRecipes.map( _.className ) )

    loop( allowedExtractedItems ++ initialItems, initialRecipes )
