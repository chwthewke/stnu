package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptySet
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import model.Item
import model.Recipe
import model.Tier
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanMsg
import spa.plan.RecipeOption
import spa.plan.RecipeOptionsInputModel

object RecipeListTab:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  private def recipePresetsBlock( options: RecipeOptionsInputModel ): Html[RecipeOption] =
    Html.div( b.panelBlock )(
      Html.div( b.field )(
        Html.div( b.buttons + b.control )(
          Html.button(
            b.button + b.isSmall + b.isInfo,
            Html.title := "all recipes except matter conversion",
            Html.onClick( RecipeOption.Reset )
          )( "Reset to default" ),
          Html.button(
            b.button + b.isSmall + b.isInfo,
            Html.onClick( RecipeOption.SetCurrent )
          )( "Current recipes" ),
          Html.div( Html.style( CSS.flexGrow( "1" ) ) )(),
          Html.label(
            Html.input(
              b.checkbox,
              Html.`type` := "checkbox",
              Option.when[Attr[Nothing]]( options.hideFicsmas )( Html.checked ),
              Html.value := options.hideFicsmas.toString,
              Html.onChange( _ => RecipeOption.ToggleHideFicsmas( !options.hideFicsmas ) )
            ),
            Html.text( "Hide FICSMAS?" )
          )
        ),
        Html.div( b.buttons + b.control )(
          Html.text( "Max tier (alts)" ) :: tierButtons( t => RecipeOption.SetMaxTier( Tier( t ), withAlts = true ) )
        ),
        Html.div( b.buttons + b.control )(
          Html.text( "Max tier (no alts)" ) ::
            tierButtons( t => RecipeOption.SetMaxTier( Tier( t ), withAlts = false ) )
        )
      )
    )

  private def tierButtons( message: Int => RecipeOption ): List[Html[RecipeOption]] =
    9.to( 0, -1 )
      .map: t =>
        Html.button( b.button + b.isSmall + b.isInfo, Html.onClick( message( t ) ) )( t.toString )
      .toList

  private val recipeBulkTogglesBlock: Html[RecipeOption] =
    Html.div( b.panelBlock )(
      Html.div( b.field + b.isHorizontal )(
        Html.div( b.buttons + b.control )(
          Html.text( "Alternates" ),
          Html.button(
            b.button + b.isSmall + b.isInfo,
            Html.onClick( RecipeOption.ToggleAlts( enable = true ) )
          )( "Add" ),
          Html.button(
            b.button + b.isSmall + b.isInfo,
            Html.onClick( RecipeOption.ToggleAlts( enable = false ) )
          )( "Remove" ),
          Html.div( Html.style( CSS.flexGrow( "1" ) ) )(),
          Html.text( "Matter conversion" ),
          Html.button(
            b.button + b.isSmall + b.isInfo,
            Html.onClick( RecipeOption.ToggleConversion( enable = true ) )
          )( "Add" ),
          Html.button(
            b.button + b.isSmall + b.isInfo,
            Html.onClick( RecipeOption.ToggleConversion( enable = false ) )
          )( "Remove" )
        )
      )
    )

  private def recipesByMainProduct(
      env: Env,
      options: RecipeOptionsInputModel
  ): List[( Item, SortedSet[Recipe.Manufacturing] )] =
    env.game.manufacturingRecipes
      .foldMap: recipe =>
        SortedMap( recipe.products.head.item -> NonEmptySet.one( recipe ) )
      .toList
      .mapFilter:
        case ( item, recipes ) =>
          val itemMatch: Boolean = options.search.terms.matches( Vector( item.displayName ) )
          val matchingRecipes: SortedSet[Recipe.Manufacturing] = recipes.filter: recipe =>
            itemMatch || options.search.terms.matches( Vector( recipe.displayName ) )

          Option.when( matchingRecipes.nonEmpty )( ( item, matchingRecipes ) )

  private def recipeListItem( env: Env, options: RecipeOptionsInputModel )(
      recipe: Recipe.Manufacturing
  ): Html[RecipeOption] =
    val isChecked: Boolean = options.allowedRecipes.contains( recipe.className )
    Html.div( b.control )(
      Html.label( b.checkbox )(
        Html.input(
          Html.`type` := "checkbox",
          Html.name   := s"res_prefs_recipe_${recipe.className}",
          Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
          Html.value := isChecked.toString,
          Html.onChange( _ => RecipeOption.SetRecipe( recipe.className, !isChecked ) )
        ),
        RecipeFrag.recipeName( env )( recipe )
      )
    )

  private def itemRecipesListItem(
      env: Env,
      options: RecipeOptionsInputModel
  )( item: Item, recipes: SortedSet[Recipe.Manufacturing] ): Html[RecipeOption] =
    Html.div( b.field )(
      Html.label( b.label )(
        icon.verticalAlign().withDropShadow().item( env, item ),
        nbsp,
        Html.text( item.displayName )
      ) ::
        recipes.toList.map( recipeListItem( env, options ) )
    )

  private def recipeListBlock( env: Env, options: RecipeOptionsInputModel ): List[Html[RecipeOption]] =
    WordsSearch( options.search, RecipeOption.SearchInput( _ ), RecipeOption.SearchReset ) ::
      recipesByMainProduct( env, options )
        .map:
          case ( item, recipes ) =>
            itemRecipesListItem( env, options )( item, recipes )

  def apply( env: Env, options: RecipeOptionsInputModel ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Presets" ) ),
      recipePresetsBlock( options ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Bulk toggles" ) ),
      recipeBulkTogglesBlock,
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Recipe list" ) ),
      Html.div( b.panelBlock, Html.styles( CSS.flexDirection( "column" ), CSS.alignItems( "stretch" ) ) )(
        recipeListBlock( env, options )
      )
    ).map( _.map( PlanMsg.SetRecipeOption( _ ) ) )
