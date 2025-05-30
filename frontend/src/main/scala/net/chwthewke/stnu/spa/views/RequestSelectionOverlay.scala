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
import model.Item
import model.Tier
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.RequestSelectionAction

object RequestSelectionOverlay:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: PlanModel ): Html[PlanMsg] =

    val itemsByTier: SortedMap[Min[Tier], NonEmptySet[Item]] =
      env.game.items
        .foldMap: item =>
          SortedMap( Min( item.tier ) -> NonEmptySet.one( item ) )

    Html.div( b.box )(
      List(
        Html.div( Html.style( CSS.display( "flex" ) ) )(
          Html.div( b.control )(
            Html.button(
              b.button + b.isDanger + b.isMedium,
              Html.onClick( PlanMsg.ToggleRequestSelection( enable = false ) )
            )(
              Html.i( p.bold.x )()
            )
          ),
          nbsp,
          Html.div( Html.style( CSS.flexGrow( "1" ) ) )(
            WordsSearch(
              model.requestSelection.search,
              value => RequestSelectionAction.SearchInput( value ),
              RequestSelectionAction.SearchReset
            ).map( PlanMsg.RequestSelection( _ ) )
          )
        ),
        Html
          .div( b.buttons )(
            itemsByTier
              .foldMap( _.toNonEmptyList.toList )
              .fproduct: item =>
                model.requestSelection.search.terms.matches[Id]( item.displayName )
              .map:
                case ( item, active ) =>
                  val buttonClasses = if ( active ) b.button + b.isLight else b.button + b.isDark + b.isStatic
                  Html
                    .button(
                      buttonClasses,
                      Html.styles( CSS.display( "inline-block" ), CSS.padding( "0.375rem 0.375rem 0 0.375rem" ) ),
                      Html.onClick( RequestSelectionAction.RequestItem( item ) )
                    )(
                      icon
                        .withSize( b.is32x32 )
                        .withDropShadow( stdDev = "2px", color = "black" )
                        .item( env, item )
                    )
          )
          .map( PlanMsg.RequestSelection( _ ) )
      )
    )
