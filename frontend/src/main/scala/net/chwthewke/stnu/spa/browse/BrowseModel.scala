package net.chwthewke.stnu
package spa
package browse

import tyrian.Cmd

case class BrowseModel(
    itemSearch: SearchQuery,
    recipeSearch: SearchQuery
):
  def update[F[_]]( message: BrowseMsg ): ( BrowseModel, Cmd[F, Nothing] ) =
    message match
      case BrowseMsg.SearchItems( terms )   => copy( itemSearch = SearchQuery.onInput( terms ) )   -> Cmd.None
      case BrowseMsg.SearchRecipes( terms ) => copy( recipeSearch = SearchQuery.onInput( terms ) ) -> Cmd.None
  def restore: BrowseModel =
    BrowseModel( itemSearch.restore, recipeSearch.restore )

object BrowseModel:
  val init: BrowseModel = BrowseModel( SearchQuery.init, SearchQuery.init )
