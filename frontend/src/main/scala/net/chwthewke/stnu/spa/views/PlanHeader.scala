package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.Elem
import tyrian.Html

import protocol.persistence.PlanName
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.PlanNameAction

object PlanHeader:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( model: PlanModel, panelButtons: Elem[PlanMsg]* ): List[Html[PlanMsg]] =
    Option.when( model.name.confirmSaving )( confirmSavePlanModal( model.name.name ) ) ++:
      Option.when( model.name.confirmNew )( confirmClearPlanModal( model.name.name ) ) ++:
      Option.when( model.name.confirmRevert )( confirmRevertPlanModal( model.name.name ) ) ++:
      List(
        Html.nav( b.level + b.mr2 )(
          Html.div( b.levelLeft )( Html.div( b.levelItem + b.buttons )( panelButtons.toList ) ),
          Html.div( b.levelItem )(
            Html.h1( b.title + b.hasTextCentered )( PlanNameEditor.apply( model.name, model.dirty ) )
          ),
          Html.div( b.levelRight )(
            Html.div( b.levelItem + b.buttons )(
              if ( model.ui.isOrganizer ) FlowsView.closeButton( model ) else FlowsView.openButton( model ),
              Html.button(
                b.button + b.isSuccess + Option.when( model.ui.isComputing )( b.isLoading ),
                Html.disabled( !model.canCompute ),
                Option.when( model.canCompute && !model.ui.isComputing )( Html.onClick( PlanMsg.SendSolverRequest ) )
              )(
                Html.span( b.iconText )(
                  Html.span( b.icon )( Html.i( p.bold.`calculator` )() ),
                  Html.span( "Compute" )
                )
              )
            )
          )
        )
      )

  private def confirmSavePlanModal( name: PlanName ): Html[PlanMsg] =
    Modal.card(
      Html.text( "Confirm overwrite?" ),
      PlanMsg.PlanName( PlanNameAction.SaveCancel )
    )(
      Html.div(
        Html.p( Html.text( show"A plan named \"" ), Html.em( name.show ), Html.text( "\" already exists." ) ),
        Html.p( show"Do you want to overwrite it?" )
      )
    )(
      List(
        ( b.isSuccess, PlanMsg.SaveRequest( confirm = true ), Html.text( "Overwrite & save" ) ),
        ( None, PlanMsg.PlanName( PlanNameAction.SaveCancel ), Html.text( "Cancel" ) )
      )
    )

  private def confirmRevertPlanModal( name: PlanName ): Html[PlanMsg] =
    Modal.card(
      Html.text( "Confirm revert?" ),
      PlanMsg.PlanName( PlanNameAction.RevertCancel )
    )(
      Html.div(
        Html.p(
          Html.text( show"Are you sure you want to revert \"" ),
          Html.em( name.show ),
          Html.text( "\" to its last saved version?" )
        )
      )
    )(
      List(
        ( b.isWarning, PlanMsg.RevertPlan, Html.text( "Revert" ) ),
        ( None, PlanMsg.PlanName( PlanNameAction.RevertCancel ), Html.text( "Cancel" ) )
      )
    )

  private def confirmClearPlanModal( name: PlanName ): Html[PlanMsg] =
    Modal.card(
      Html.text( "Confirm new plan?" ),
      PlanMsg.PlanName( PlanNameAction.ClearCancel )
    )(
      Html.div(
        Html.p(
          Html.text( show"Are you sure you want to discard your changes to \"" ),
          Html.em( name.show ),
          Html.text( "\" to start a new plan?" )
        )
      )
    )(
      List(
        ( b.isWarning, PlanMsg.ClearPlan, Html.text( "Discard & new plan" ) ),
        ( None, PlanMsg.PlanName( PlanNameAction.ClearCancel ), Html.text( "Cancel" ) )
      )
    )
