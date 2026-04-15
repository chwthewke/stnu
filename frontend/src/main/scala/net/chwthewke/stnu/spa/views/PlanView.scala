package net.chwthewke.stnu
package spa
package views

import tyrian.CSS
import tyrian.Elem
import tyrian.Html

import spa.css.Bulma
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.prod.Flows

object PlanView:
  val b: Bulma = Bulma

  def apply( env: Env, model: PlanModel ): Html[PlanMsg] =
    val realEnv: Env = if ( model.recipeOptions.hideFicsmas ) env.withoutFicsmas else env
    def contents( buttons: Html[PlanMsg]* ): List[Html[PlanMsg]] =
      Elements.goToTop ::
        ( if ( model.ui.isOrganizer )
            FlowsView( model, buttons* )
          else
            mainPlanContent( realEnv, model, buttons* ) )

    if ( model.ui.isRequests )
      Html.div( b.columns + b.mx3 )(
        Html.div( b.column + b.isOneQuarter )( PlanRequestsView.requestsPanel( model ) ),
        Html.div( b.column + b.isThreeQuarters )(
          contents( PlanOptionsView.optionsButton( model ) )
        )
      )
    else if ( model.ui.options.isDefined )
      Html.div( b.columns + b.mx3 )(
        Html.div( b.column + b.isOneQuarter )( PlanOptionsView.optionsPanel( realEnv, model ) ),
        Html.div( b.column + b.isThreeQuarters )(
          contents( PlanRequestsView.requestsButton( model ) )
        )
      )
    else
      Html.div( b.columns + b.mx3 )(
        Html.div( b.column )(
          contents( PlanRequestsView.requestsButton( model ), PlanOptionsView.optionsButton( model ) )
        )
      )

  private def requestSelectionModal( env: Env, model: PlanModel ): Html[PlanMsg] =
    Modal(
      PlanMsg.ToggleRequestSelection( enable = false ),
      Html.style( CSS.width( "50%" ) )
    )( RequestSelectionOverlay( env, model ) )

  private def mainPlanContent( env: Env, model: PlanModel, leftButtons: Elem[PlanMsg]* ): List[Html[PlanMsg]] =
    Option.when( model.ui.requestSelectionVisible )( requestSelectionModal( env, model ) ) ++:
      PlanHeader( model, leftButtons* ) ++:
      PlanTable( model.productionUi, model.flows.getOrElse( Flows.init( model.production ) ) ) +:
      Nil
