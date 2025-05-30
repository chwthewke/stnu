package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*

import model.Recipe
import model.Tier

case class RecipeOptionsInputModel(
    hideFicsmas: Boolean,
    allowedRecipes: Set[ClassName[Recipe.Manufacturing]],
    search: SearchQuery
):
  def setOption( env: Env, option: RecipeOption ): RecipeOptionsInputModel =
    option match
      case RecipeOption.Reset => copy( allowedRecipes = RecipeOptionsInputModel.defaultRecipes( env ) )
      case RecipeOption.SetMaxTier( tier, withAlts ) =>
        copy( allowedRecipes = RecipeOptionsInputModel.allRecipesUpToTier( env, tier, withAlts ) )
      case RecipeOption.ToggleAlts( enable ) =>
        copy(allowedRecipes =
          if ( enable ) allowedRecipes ++ RecipeOptionsInputModel.allAlternateRecipes( env )
          else allowedRecipes.diff( RecipeOptionsInputModel.allAlternateRecipes( env ) )
        )
      case RecipeOption.ToggleConversion( enable ) =>
        copy(allowedRecipes =
          if ( enable ) allowedRecipes ++ RecipeOptionsInputModel.allConversionRecipes( env )
          else allowedRecipes.diff( RecipeOptionsInputModel.allConversionRecipes( env ) )
        )
      case RecipeOption.SetRecipe( recipe, enable ) =>
        copy( allowedRecipes = if ( enable ) allowedRecipes + recipe else allowedRecipes - recipe )
      case RecipeOption.SearchInput( value )        => copy( search = search.onInput( value ) )
      case RecipeOption.SearchReset                 => copy( search = search.clear )
      case RecipeOption.ToggleHideFicsmas( enable ) => copy( hideFicsmas = enable )

  def restore: RecipeOptionsInputModel =
    copy( search = search.restore )

object RecipeOptionsInputModel:
  def init( env: Env ): RecipeOptionsInputModel =
    RecipeOptionsInputModel( hideFicsmas = false, defaultRecipes( env ), SearchQuery.init )
  private def defaultRecipes( env: Env ): Set[ClassName[Recipe.Manufacturing]] =
    env.manufacturingRecipesWhere( !_.isMatterConversion )
  private def allRecipesUpToTier( env: Env, tier: Tier, withAlts: Boolean ): Set[ClassName[Recipe.Manufacturing]] =
    env.manufacturingRecipesWhere: recipe =>
      recipe.category.tier <= tier && ( withAlts || !recipe.isAlternate )
  private def allAlternateRecipes( env: Env ): Set[ClassName[Recipe.Manufacturing]] =
    env.manufacturingRecipesWhere( _.isAlternate )
  private def allConversionRecipes( env: Env ): Set[ClassName[Recipe.Manufacturing]] =
    env.manufacturingRecipesWhere( _.isMatterConversion )
