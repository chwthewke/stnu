package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.SidePanel

object PlanOptionsView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def optionsButton( model: PlanModel ): Html[Nothing] =
    Html.a( b.button + b.isInfo, Html.href := model.locationToOpenOptions.toInternalLocation )(
      Html.span( b.iconText )( Html.span( "Options" ), Html.span( b.icon )( Html.i( p.regular.arrowsOut )() ) )
    )

  private def optionsPanelHeader( model: PlanModel ): Html[Nothing] =
    Html.div( b.panelHeading + b.p2, Html.style( CSS.display( "flex" ) ) )(
      Html.span( Html.style( CSS.flexGrow( "1" ) ) )( "Options" ),
      Html.a( b.hasTextInfoDark, Html.href := model.locationToCloseOptions.toInternalLocation )(
        Html.i( p.regular.arrowsIn )()
      )
    )

  private def optionTabs( model: PlanModel ): Html[Nothing] =
    Html.p( b.panelTabs )(
      SidePanel.values
        .filter( _.hasOptions )
        .toList
        .map( option =>
          Html.a(
            Option.when[Attr[Nothing]]( option == model.ui.hasOptions )( b.isActive ),
            Html.href := model.locationToOpenOption( option ).toInternalLocation
          )( option.description )
        )
    )

  private val tabs: Vector[( SidePanel.OptionsTab, ( Env, PlanModel ) => List[Html[PlanMsg]] )] =
    Vector(
      ( SidePanel.Recipes, ( env, m ) => RecipeListTab( env, m.recipeOptions ) ),
      ( SidePanel.ResourceNodes, ( env, m ) => ResourceNodesTab( env, m.resourceOptions ) ),
      ( SidePanel.ResourcePrefs, ( env, m ) => ExtractionOptionsTab( env, m.extractionOptions ) ),
      ( SidePanel.Logistics, ( env, m ) => LogisticsOptionsTab( env, m.logisticsOptions ) ),
      ( SidePanel.Power, ( env, m ) => PowerOptionsTab( env, m.powerOptions ) )
    )

  private def selectedOptionsTab( env: Env, model: PlanModel ): List[Html[PlanMsg]] =
    tabs
      .find:
        case ( tab, _ ) => model.ui.hasOptions == tab
      .foldMap:
        case ( _, mkView ) => mkView( env, model )

  def optionsPanel( env: Env, model: PlanModel ): Html[PlanMsg] =
    Html.div( b.panel + b.isInfo )(
      optionsPanelHeader( model )
        :: optionTabs( model )
        :: selectedOptionsTab( env, model )
    )
