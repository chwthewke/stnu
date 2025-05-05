package net.chwthewke.stnu
package spa
package views

import cats.Traverse
import cats.syntax.all.*
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import data.Countable
import model.Item
import model.Recipe
import spa.browse.BrowseModel
import spa.browse.BrowseMsg
import spa.css.Bulma
import spa.css.Phosphor

object BrowseView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: BrowseModel ): Html[BrowseMsg] =
    val realEnv: Env = if ( model.hideFicsmas ) env.withoutFicsmas else env
    Html.div( b.columns )(
      Html.div( b.column + b.isTwoFifths )( browseItems( realEnv, model.itemSearch ) ),
      Html.div( b.column + b.isThreeFifths )( browseRecipes( realEnv, model ) )
    )

  def browseItems( env: Env, itemSearch: SearchQuery ): Html[BrowseMsg] =
    Html.div( b.box + b.m2 )(
      Html.h2( b.title )( "Items" ),
      SearchableItemTable.displayList( env )( env.game.items.values.toList.sortBy( _.displayName ), itemSearch )
    )

  def browseRecipes( env: Env, model: BrowseModel ): Html[BrowseMsg] =
    Html.div( b.box + b.m2 )(
      Html.h2( b.title )(
        Html.text( "Recipes" ),
        Html.span( b.field + b.isHorizontal + b.isInlineFlex + b.ml2 )(
          Html.span( b.control + b.mx1 )(
            Html.text( "Sort" ),
            nbsp,
            Html.label( b.radio )(
              Html.input(
                Html.`type` := "radio",
                Html.name   := "browse_recipes_sort",
                Option.when[Attr[Nothing]]( model.sort == BrowseModel.Sort.Name )( Html.checked ),
                Html.onChange( _ => BrowseMsg.Sort( BrowseModel.Sort.Name ) )
              ),
              nbsp,
              Html.text( "by name" )
            )
          ),
          Html.span( b.control + b.mx1 )(
            Html.label( b.radio )(
              Html.input(
                Html.`type` := "radio",
                Html.name   := "browse_recipes_sort",
                Option.when[Attr[Nothing]]( model.sort == BrowseModel.Sort.Topo )( Html.checked ),
                Html.onChange( _ => BrowseMsg.Sort( BrowseModel.Sort.Topo ) )
              ),
              nbsp,
              Html.text( "topo" )
            )
          )
        ),
        Html.span( b.field + b.isHorizontal + b.isInlineFlex + b.ml2 )(
          Html.span( b.control + b.mx1 )(
            Html.label( b.checkbox )(
              Html.input(
                Html.`type` := "checkbox",
                Option.when[Attr[Nothing]]( model.hideFicsmas )( Html.checked ),
                Html.onChange( _ => BrowseMsg.ToggleHideFicsmas( !model.hideFicsmas ) )
              ),
              nbsp,
              Html.text( "Hide FICSMAS" )
            )
          )
        )
      ),
      SearchableRecipeTable.displayList( env )( model.sortRecipes( env ), model.recipeSearch )
    )

  trait SearchableTable[A, +M]:

    type F[_]
    given F: Traverse[F] = compiletime.deferred

    type K
    given O: Ordering[K] = compiletime.deferred

    def id: String
    def searchTargets( item: A ): F[String]
    def displayItem( env: Env )( item: A ): List[Html[M]]
    def itemRowAttrs( env: Env )( item: A ): List[Attr[M]] = Nil
    def inputMsg( searchInput: String ): M
    def resetMsg: M

    private def searchHit( searchTerms: SearchTerms, item: A ): Boolean =
      searchTerms.matches( searchTargets( item ) )

    def displayList( env: Env )( items: Iterable[A], search: SearchQuery ): Html[M] =
      Html.div(
        WordsSearch( search, inputMsg, resetMsg ),
        Html.div( Html.styles( CSS.height( "calc(100vh - 15rem)" ), CSS.overflowY( "scroll" ) ) )(
          Html.table( b.table + b.isFullwidth )(
            Html.tbody(
              items
                .filter( searchHit( search.terms, _ ) )
                .toList
                .map: item =>
                  Html.tr( itemRowAttrs( env )( item ) )( displayItem( env )( item ) )
            )
          )
        )
      )

  object SearchableItemTable extends SearchableTable[Item, BrowseMsg]:
    type F[x] = x

    override def id: String = "search_items"

    override def searchTargets( item: Item ): String = item.displayName

    override type K = String

    override def displayItem( env: Env )( item: Item ): List[Html[Nothing]] =
      List(
        Html.td( icon.verticalAlign().withDropShadow().item( env, item ) ),
        Html.td( item.displayName )
      )

    override def itemRowAttrs( env: Env )( item: Item ): List[Attr[BrowseMsg]] =
      List(
        Html.styles( CSS.cursor( "pointer" ), CSS.verticalAlign( "middle" ) ),
        Html.onClick( BrowseMsg.SelectItem( item.displayName ) )
      )

    override def inputMsg( searchInput: String ): BrowseMsg = BrowseMsg.SearchItems( searchInput )

    override def resetMsg: BrowseMsg = BrowseMsg.ResetItemSearch

  object SearchableRecipeTable extends SearchableTable[Recipe.NonExtraction, BrowseMsg]:
    type F[x] = Vector[x]

    override def id: String = "search_recipes"

    override def searchTargets( recipe: Recipe.NonExtraction ): Vector[String] =
      recipe.displayName +: recipe.itemsPerMinute.map( _.item.displayName )

    override type K = String

    override def itemRowAttrs( env: Env )(
        recipe: Recipe.NonExtraction
    ): List[Attr[BrowseMsg]] =
      List( Html.styles( CSS.verticalAlign( "middle" ) ) )

    def numberedItem( env: Env, ci: Countable[Double, Item] ): Html[Nothing] =
      Html.span(
        b.px2 + b.hasTextWeightBold,
        Html.styles( CSS.verticalAlign( "middle" ), CSS.minHeight( "24px" ), CSS.display( "inline-flex" ) )
      )(
        Html.div( Html.styles( CSS.verticalAlign( "middle" ) ) )(
          Numbers.showDouble1( ci.amount )
        ),
        nbsp,
        icon.verticalAlign().withDropShadow().item( env, ci.item )
      )

    override def displayItem( env: Env )(
        recipe: Recipe.NonExtraction
    ): List[Html[BrowseMsg]] =
      recipe match
        case power: Recipe.PowerGeneration => displayPowerGenerationRecipe( env, power )
        case manu: Recipe.Manufacturing    => displayManufacturingRecipe( env, manu )

    private def displayPowerGenerationRecipe( env: Env, recipe: Recipe.PowerGeneration ) =
      displayRecipe( env, recipe ): products =>
        Html.td( Html.styles( CSS.textAlign( "left" ), CSS.verticalAlign( "middle" ) ) )(
          Html.span( b.hasTextWeightBold, Html.style( CSS.verticalAlign( "middle" ) ) )(
            Numbers.showDouble1( -recipe.power.average ) + " MW"
          ) ::
            nbsp ::
            products.map( numberedItem( env, _ ) )
        )

    private def displayManufacturingRecipe( env: Env, recipe: Recipe.Manufacturing ) =
      displayRecipe( env, recipe ): products =>
        Html.td( Html.styles( CSS.textAlign( "left" ), CSS.verticalAlign( "middle" ) ) )(
          products.toList.map( numberedItem( env, _ ) )
        )

    private def displayRecipe[R <: Recipe]( env: Env, recipe: R )(
        displayProducts: recipe.P[Countable[Double, Item]] => Html[Nothing]
    ): List[Html[BrowseMsg]] =
      List(
        Html.td( Html.styles( CSS.verticalAlign( "middle" ) ) )(
          Html.text( recipe.displayName ), {
            Html.span( b.mx1 + b.tag + b.isPrimary )( show"Tier ${recipe.category.tier}" )
          },
          Option.when( recipe.isAlternate )(
            Html.span( b.mx1 + b.tag + b.isInfo )( "Alt" )
          ),
          Option.when( recipe.isMatterConversion )(
            Html.span( b.mx1 + b.tag + b.isLink )( "MC" )
          ),
          Option.when( recipe.category.powerGeneration.nonEmpty )(
            Html.span( b.mx1 + b.tag + b.isSuccess )( "Power" )
          )
        ),
        Html.td( Html.styles( CSS.textAlign( "right" ), CSS.verticalAlign( "middle" ) ) )(
          Html.span()(
            recipe.ingredients.map( numberedItem( env, _ ) )
          )
        ),
        Html.td( Html.styles( CSS.textAlign( "center" ), CSS.verticalAlign( "middle" ) ) )(
          Html.span( b.px1 + b.icon )( Html.i( p.fill.arrowFatRight )() )
        ),
        displayProducts( recipe.products ),
        Html.td( Html.styles( CSS.verticalAlign( "middle" ) ) )(
          Html.span(
            s"${Numbers.showDouble1( recipe.duration.toMillis / 1000d )} s"
          )
        )
      )

    override def inputMsg( searchInput: String ): BrowseMsg = BrowseMsg.SearchRecipes( searchInput )

    override def resetMsg: BrowseMsg = BrowseMsg.ResetRecipeSearch
