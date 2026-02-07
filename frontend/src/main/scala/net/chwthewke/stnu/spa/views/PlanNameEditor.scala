package net.chwthewke.stnu
package spa.views

import cats.syntax.all.*
import tyrian.CSS
import tyrian.Html

import spa.InputModel
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanMsg
import spa.plan.PlanNameAction
import spa.plan.PlanNameModel

object PlanNameEditor:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( model: PlanNameModel, dirty: Boolean ): Html[PlanMsg] =

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
          )( Html.span( b.iconText )( Html.span( b.icon )( Html.i( p.regular.`pencil` )() ), Html.span( "Rename" ) ) ),
          if ( dirty )
            Html.button(
              b.button + b.isSuccess + b.mr2,
              Html.onClick( PlanMsg.SaveRequest( confirm = false ) )
            )(
              Html.span( b.iconText )( Html.span( b.icon )( Html.i( p.regular.`floppyDisk` )() ), Html.span( "Save" ) )
            )
          else
            Html
              .button(
                b.button + b.isSuccess + b.mr2,
                Html.onClick( PlanNameAction.Duplicate )
              )(
                Html.span( b.iconText )( Html.span( b.icon )( Html.i( p.regular.`copy` )() ), Html.span( "Duplicate" ) )
              )
              .map( PlanMsg.PlanName( _ ) ),
          Html.button(
            b.button + b.isInfo + b.mr2,
            Html.onClick( PlanNameAction.Revert ).map( PlanMsg.PlanName( _ ) ),
            Option.when( !dirty || model.saved.isEmpty )( Html.disabled )
          )(
            Html.span( b.iconText )( Html.span( b.icon )( Html.i( p.regular.arrowUUpLeft )() ), Html.span( "Revert" ) )
          ),
          Html.button(
            b.button + b.isInfo + b.mr2,
            Html.onClick:
              if ( dirty )
                PlanMsg.PlanName( PlanNameAction.Clear )
              else
                PlanMsg.ClearPlan
          )( Html.span( b.iconText )( Html.span( b.icon )( Html.i( p.regular.`filePlus` )() ), Html.span( "New" ) ) )
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
