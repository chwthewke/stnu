package net.chwthewke.stnu
package spa
package browse

import tyrian.Cmd

import model.Recipe
import spa.browse.BrowseModel.Sort

case class BrowseModel(
    itemSearch: SearchQuery,
    recipeSearch: SearchQuery,
    sort: BrowseModel.Sort,
    hideFicsmas: Boolean
):
  def update[F[_]]( message: BrowseMsg ): ( BrowseModel, Cmd[F, Nothing] ) =
    message match
      case BrowseMsg.SearchItems( terms )           => copy( itemSearch = itemSearch.onInput( terms ) )     -> Cmd.None
      case BrowseMsg.ResetItemSearch                => copy( itemSearch = itemSearch.clear )                -> Cmd.None
      case BrowseMsg.SearchRecipes( terms )         => copy( recipeSearch = recipeSearch.onInput( terms ) ) -> Cmd.None
      case BrowseMsg.ResetRecipeSearch              => copy( recipeSearch = recipeSearch.clear )            -> Cmd.None
      case BrowseMsg.SelectItem( name: String )     => copy( recipeSearch = SearchQuery( s""""$name"""" ) ) -> Cmd.None
      case BrowseMsg.Sort( sort: BrowseModel.Sort ) => copy( sort = sort )                                  -> Cmd.None
      case BrowseMsg.ToggleHideFicsmas( enable )    => copy( hideFicsmas = enable )                         -> Cmd.None

  def restore: BrowseModel =
    BrowseModel( itemSearch.restore, recipeSearch.restore, sort, hideFicsmas )

  def sortRecipes( env: Env ): List[Recipe.NonExtraction] =
    val realEnv = if ( hideFicsmas ) env.withoutFicsmas else env
    sort match
      case Sort.Name => realEnv.nonExtractionRecipes.sortBy( _.displayName ).toList
      case Sort.Topo => realEnv.sortRecipes.toList

object BrowseModel:
  val init: BrowseModel = BrowseModel( SearchQuery.init, SearchQuery.init, Sort.Name, hideFicsmas = false )

  enum Sort:
    case Name
    case Topo
