package net.chwthewke.stnu
package spa
package views

import cats.Id
import cats.data.NonEmptySet
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import tyrian.CSS
import tyrian.Html

import data.newts.Min
import model.ExtractorType
import model.Feasible
import model.Item
import model.Machine
import model.Recipe
import model.ResourceDistrib
import model.Tier
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.RequestsAction

object RequestSelectionOverlay:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def feasible(
      env: Env,
      allowedRecipes: Set[ClassName[Recipe.Manufacturing]],
      resourceDistributions: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
      extractors: Set[ExtractorType],
      generators: Set[ClassName[Machine]]
  ): Set[ClassName[Item]] =
    val extracted: Set[ClassName[Item]] =
      env.game.extractionRecipes
        .filter:
          case ( ( item, machine ), recipes ) =>
            machine.machineType.extractor.exists( extractors.contains_( _ ) ) &&
            machine.machineType.extractor
              .flatMap( resourceDistributions.get )
              .flatMap( _.get( item.className ) )
              .forall( _.foldMap( ( _, n ) => n ) > 0 )
        .keySet
        .map( _._1.className )

    val allowRecipe: Recipe.NonExtraction => Boolean = {
      case man: Recipe.Manufacturing   => allowedRecipes.contains( man.className )
      case pow: Recipe.PowerGeneration => generators.contains( pow.producedIn.className )
    }

    Feasible(
      env.game,
      allowExtractedItem = ( item: Item ) => extracted.contains_( item.className ),
      allowRecipe = allowRecipe
    )._1

  def apply( env: Env, model: PlanModel ): Html[PlanMsg] =

    val itemsByTier: SortedMap[Min[Tier], NonEmptySet[Item]] =
      env.game.items
        .foldMap: item =>
          SortedMap( Min( item.tier ) -> NonEmptySet.one( item ) )

    val feasibleItems: Set[ClassName[Item]] =
      feasible(
        env,
        model.recipeOptions.allowedRecipes,
        model.resourceOptions.resourceNodes,
        model.extractionOptions.extractors,
        model.powerOptions.allowedGenerators
      )

    Html.div( b.box )(
      List(
        Html.div( Html.style( CSS.display( "flex" ) ) )(
          Html.div( b.control )(
            Html.button(
              b.button + b.isMedium,
              Html.onClick( PlanMsg.ToggleRequestSelection( enable = false ) )
            )(
              Html.i( p.bold.x )()
            )
          ),
          nbsp,
          Html.div( Html.style( CSS.flexGrow( "1" ) ) )(
            WordsSearch(
              model.requests.search,
              value => RequestsAction.SearchInput( value ),
              RequestsAction.SearchReset
            ).map( PlanMsg.Requests( _ ) )
          )
        ),
        Html
          .div( b.buttons )(
            itemsByTier
              .foldMap( _.toNonEmptyList.toList )
              .fproduct: item =>
                model.requests.search.terms.matches[Id]( item.displayName )
                  && feasibleItems.contains( item.className )
              .map:
                case ( item, active ) =>
                  val selected      = model.requests.requestedItems.contains( item.className )
                  val buttonClasses = if ( active ) b.button + b.isLight else b.button + b.isDark + b.isStatic
                  Html
                    .button(
                      buttonClasses,
                      Html.styles( CSS.display( "inline-block" ), CSS.padding( "0.375rem 0.375rem 0 0.375rem" ) ),
                      Html.onClick( RequestsAction.RequestItem( item ) )
                    )(
                      icon
                        .withSize( b.is32x32 )
                        .withDropShadow( stdDev = "2px", color = "black" )
                        .item( env, item ),
                      Option.when( selected )(
                        Html.i(
                          b.hasTextSuccess + p.fill.checkFat,
                          Html.styles(
                            CSS.position( "absolute" ),
                            CSS.right( "0px" ),
                            CSS.bottom( "0px" ),
                            CSS.zIndex( "10" )
                          )
                        )()
                      )
                    )
          )
          .map( PlanMsg.Requests( _ ) )
      )
    )
