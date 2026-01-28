package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.CSS
import tyrian.Html

import protocol.persistence.PlanName
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.PlanNameAction
import spa.plan.PlanNameModel

object PlanView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: PlanModel ): Html[PlanMsg] =
    val realEnv: Env = if ( model.recipeOptions.hideFicsmas ) env.withoutFicsmas else env
    if ( model.ui.optionsOpen )
      Html.div( b.columns )(
        Html.div( b.column + b.isOneQuarter )( PlanOptionsView.optionsPanel( realEnv, model ) ),
        Html.div( b.column + b.isThreeQuarters )( mainPlanContent( realEnv, model ) )
      )
    else
      Html.div(
        PlanOptionsView.collapsedOptionsPanel( model ) ::
          mainPlanContent( realEnv, model )
      )

  def planNameOrEditor( model: PlanNameModel, dirty: Boolean ): Html[PlanMsg] =

    def planNameWithEditButton: List[Html[PlanMsg]] =
      Html.span( b.mb2, Html.style( CSS.display( "inline-block" ) ) )(
        Html.span(
          Html.style( CSS.verticalAlign( "baseline" ) ),
          Option.when( dirty )( b.isItalic )
        )(
          model.name.show
        ),
        Html.span( Html.style( CSS.verticalAlign( "baseline" ) ), b.ml2 )(
          Html.button(
            b.button + b.mr2,
            Html.onClick( PlanNameAction.EditStart ).map( PlanMsg.PlanName( _ ) )
          )( Html.i( p.regular.`pencil` )(), Html.text( "Rename" ) ),
          Html.button(
            b.button + b.isSuccess + b.mr2,
            Html.onClick( PlanMsg.SaveRequest( confirm = false ) ),
            Option.when( !dirty )( Html.disabled )
          )( Html.i( p.regular.`floppyDisk` )(), Html.text( "Save" ) ),
          Html.button(
            b.button + b.isInfo + b.mr2,
            Html.onClick( PlanNameAction.Revert ).map( PlanMsg.PlanName( _ ) ),
            Option.when( !dirty || model.saved.isEmpty )( Html.disabled )
          )( Html.i( p.regular.arrowUUpLeft )(), Html.text( "Revert" ) ),
          Html.button(
            b.button + b.isInfo + b.mr2,
            Html.onClick:
              if ( dirty )
                PlanMsg.PlanName( PlanNameAction.Clear )
              else
                PlanMsg.ClearPlan
          )( Html.i( p.regular.`filePlus` )(), Html.text( "New" ) )
        )
      )
        :: Nil

    def planNameEditor( input: InputModel ): List[Html[PlanNameAction]] =
      Html.div( b.field + b.hasAddons )(
        Html.div( b.control )(
          Html
            .div(
              b.button + b.isMedium + b.isInfo,
              Html.style( CSS.height( "var(--bulma-control-height)" ) ),
              Html.onClick( PlanNameAction.EditCancel )
            )( Html.i( p.regular.`arrowUUpLeft` )() )
        ),
        Html
          .div( b.control )(
            Html.input(
              b.input + b.isMedium,
              Html.id     := PlanNameModel.editorId,
              Html.`type` := "text",
              input.output.map( v => Html.value := v ),
              Html.onInput( value => PlanNameAction.EditSetValue( value ) )
            )
          ),
        Html
          .div( b.control )(
            Html.div(
              b.button + b.isMedium + b.isSuccess,
              Html.style( CSS.height( "var(--bulma-control-height)" ) ),
              Html.onClick( PlanNameAction.EditCommit )
            )( Html.i( p.regular.check )() )
          )
      ) :: Nil

    Html
      .p( b.title )(
        model.input.fold( planNameWithEditButton )( ed => planNameEditor( ed ).map( _.map( PlanMsg.PlanName( _ ) ) ) )
      )

  private def confirmSavePlanModal( name: PlanName ): Html[PlanMsg] =
    CardModal(
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
    CardModal(
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
    CardModal(
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

  private def mainPlanContent( env: Env, model: PlanModel ): List[Html[PlanMsg]] =
    // TODO bulma modal background for the request selection modal?
    //   (I think the modal itself cannot be bulma bc of its desired position)
    Option.when( model.ui.requestSelectionVisible )(
      Html.div(
        Html.styles(
          CSS.position( "absolute" ),
          CSS.top( "0" ),
          CSS.bottom( "0" ),
          CSS.left( "0" ),
          CSS.right( "0" ),
          CSS.backgroundColor( "rgb( from black r g b / 50% )" ),
          CSS.zIndex( "10" )
        ),
        Html.onClick( PlanMsg.ToggleRequestSelection( enable = false ) )
      )()
    ) ++:
      Option.when( model.name.confirmSaving )( confirmSavePlanModal( model.name.name ) ) ++:
      Option.when( model.name.confirmNew )( confirmClearPlanModal( model.name.name ) ) ++:
      Option.when( model.name.confirmRevert )( confirmRevertPlanModal( model.name.name ) ) ++:
      List(
        Html.section( b.hero + b.isPrimary )(
          Html.div( b.heroBody )(
            planNameOrEditor( model.name, model.dirty ),
            Html.p( b.subtitle )( "Request products and plan machines and transport" ),
            Html.div( b.box, Html.styles( CSS.position( "relative" ) ) )(
              Html.div( b.field + b.isGrouped + b.isAlignItemsCenter, Html.style( CSS.marginBottom( "0" ) ) )(
                Html.div( b.control )(
                  Html
                    .button( b.button + b.isSuccess, Html.onClick( PlanMsg.ToggleRequestSelection( enable = true ) ) )(
                      Html.span( b.iconText )(
                        Html.span( b.icon )( Html.i( p.bold.`plusSquare` )() ),
                        Html.span( "Add/edit request" )
                      )
                    )
                ),
                Html.div( b.control + b.isExpanded )(
                  Html.button(
                    b.button + b.isSuccess + Option.when( !model.canCompute )( b.isStatic ),
                    Html.onClick( PlanMsg.SendSolverRequest )
                  )(
                    Html.span( b.iconText )(
                      Html.span( b.icon )( Html.i( p.bold.`calculator` )() ),
                      Html.span( "Compute" )
                    )
                  )
                ),
                Option.when( model.canCompute )(
                  Html.div( b.control + b.isExpanded )( Html.em( "Request modified, compute to update plan" ) )
                ),
                Html.div( b.control + b.isRight )(
                  Html.label( b.checkbox )(
                    Html.input( Html.`type` := "checkbox", Html.disabled ),
                    nbsp,
                    Html.text( "Recompute automatically?" )
                  )
                )
              ),
              Option.when( model.ui.requestSelectionVisible )(
                Html.div(
                  Html.styles(
                    CSS.position( "absolute" ),
                    CSS.marginTop( "0.75rem" ),
                    CSS.display( "flex" ),
                    CSS.zIndex( "11" )
                  )
                )(
                  Html.div( Html.styles( CSS.flexGrow( "1" ) ) )(),
                  Html.div( Html.style( CSS.maxWidth( "90%" ) ) )(
                    RequestSelectionOverlay( env, model )
                  ),
                  Html.div( Html.styles( CSS.flexGrow( "1" ) ) )()
                )
              )
            )
          )
        ),
        // TODO tabs here probably
        PlanTable( model.ui, model.production )
      )
