package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.Recipe
import spa.plan.RecipeOptionsInputModel

object LocalRecipeOptions:
  case class Saved(
      hideFicsmas: Boolean,
      allowedRecipes: Set[ClassName[Recipe.Manufacturing]],
      search: Option[String]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( recipeOptions: RecipeOptionsInputModel ): Saved =
      Saved(
        recipeOptions.hideFicsmas,
        recipeOptions.allowedRecipes,
        fromSearchQuery( recipeOptions.search )
      )

  case class Loaded(
      hideFicsmas: Boolean,
      allowedRecipes: Set[ClassName[Recipe.Manufacturing]],
      search: Option[String]
  ) derives ConfiguredDecoder:
    def toRecipeOptions: RecipeOptionsInputModel =
      RecipeOptionsInputModel(
        hideFicsmas,
        allowedRecipes,
        toSearchQuery( search )
      )
