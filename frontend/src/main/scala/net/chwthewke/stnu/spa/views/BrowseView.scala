package net.chwthewke.stnu
package spa
package views

import cats.Traverse
import cats.syntax.all.*
import tyrian.CSS
import tyrian.Html

import data.Countable
import model.Item
import model.Recipe
import spa.browse.BrowseModel
import spa.browse.BrowseMsg
import spa.browse.SearchQuery
import spa.browse.SearchTerms
import spa.css.Bulma
import spa.css.Phosphor

object BrowseView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: BrowseModel ): Html[BrowseMsg] =
    Html.div( b.columns )(
      Html.div( b.column + b.isTwoFifths )( browseItems( env, model.itemSearch ) ),
      Html.div( b.column + b.isThreeFifths )( browseRecipes( env, model.recipeSearch ) )
    )

  def browseItems( env: Env, itemSearch: SearchQuery ): Html[BrowseMsg] =
    Html.div( b.box + b.m2 )(
      Html.h2( b.title )( "Items" ),
      SearchableItemTable.displayList( env )( env.game.items.values.toList, itemSearch )
    )

  def browseRecipes( env: Env, recipeSearch: SearchQuery ): Html[BrowseMsg] =
    Html.div( b.box + b.m2 )(
      Html.h2( b.title )( "Recipes" ),
      SearchableRecipeTable.displayList( env )( env.game.manufacturingRecipes.toList, recipeSearch )
    )

  trait SearchableTable[A, +M]:

    type F[_]
    given F: Traverse[F] = compiletime.deferred

    type K
    given O: Ordering[K] = compiletime.deferred

    def id: String
    def searchTargets( item: A ): F[String]
    def sortKey( item: A ): K
    def displayItem( env: Env )( item: A ): List[Html[M]]
    def inputMsg( searchInput: String ): M

    private def searchHit( searchTerms: SearchTerms, item: A ): Boolean =
      val targets: List[String] = searchTargets( item ).toList
      searchTerms
        .map( _.toLowerCase )
        .forall: term =>
          targets.exists( _.toLowerCase.contains( term ) )

    def displayList( env: Env )( items: Iterable[A], search: SearchQuery ): Html[M] =
      Html.div(
        Html.div( b.control + b.hasIconsLeft )(
          Html.input(
            b.input,
            Html.id          := id,
            Html.placeholder := "Search",
            Html.onInput( str => inputMsg( str ) ),
            search.output.map( v => Html.value := v )
          ),
          Html.span( b.icon + b.isLeft )(
            Html.i( p.regular.`magnifyingGlass` )()
          )
        ),
        Html.table( b.table + b.isFullwidth )(
          Html.tbody(
            items
              .filter( searchHit( search.terms, _ ) )
              .toList
              .sortBy( sortKey )
              .map: item =>
                Html.tr( displayItem( env )( item ) )
          )
        )
      )

  object SearchableItemTable extends SearchableTable[Item, BrowseMsg]:
    type F[x] = x

    override def id: String = "search_items"

    override def searchTargets( item: Item ): String = item.displayName

    override type K = String

    override def sortKey( item: Item ): String = item.displayName

    override def displayItem( env: Env )( item: Item ): List[Html[Nothing]] =
      List(
        Html.td( Icons.item( env, item ) ),
        Html.td( item.displayName )
      )

    override def inputMsg( searchInput: String ): BrowseMsg = BrowseMsg.SearchItems( searchInput )

  object SearchableRecipeTable extends SearchableTable[Recipe.Prod, BrowseMsg]:
    type F[x] = Vector[x]

    override def id: String = "search_recipes"

    override def searchTargets( recipe: Recipe.Prod ): Vector[String] =
      recipe.displayName +: recipe.itemsPerMinute.map( _.item.displayName )

    override type K = String

    override def sortKey( item: Recipe.Prod ): String = item.displayName

    def numberedItem( env: Env, ci: Countable[Double, Item] ): Html[Nothing] =
      Html.span( b.px1 + b.hasTextWeightBold + b.isSize5 )(
        Html.span( Numbers.showDouble1( ci.amount ) ),
        Icons.item( env, ci.item )
      )

    override def displayItem( env: Env )( recipe: Recipe.Prod ): List[Html[BrowseMsg]] =
      List(
        Html.td( recipe.displayName ),
        Html.td( Html.style( CSS.textAlign( "right" ) ) )(
          Html.span( recipe.ingredients.map( numberedItem( env, _ ) ) )
        ),
        Html.td( Html.style( CSS.textAlign( "center" ) ) )(
          Html.span( b.px1 + b.icon + b.hasTextSuccess + b.isSize4 )( Html.i( p.fill.arrowFatRight )() )
        ),
        Html.td( Html.style( CSS.textAlign( "left" ) ) )(
          Html.span( recipe.products.toList.map( numberedItem( env, _ ) ) )
        ),
        Html.td(
          Html.span( b.isSize5 + b.hasTextWeightBold )(
            s"${Numbers.showDouble1( recipe.duration.toMillis / 1000d )} s"
          )
        )
      )

    override def inputMsg( searchInput: String ): BrowseMsg = BrowseMsg.SearchRecipes( searchInput )
