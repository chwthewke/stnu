package net.chwthewke.stnu
package spa
package views

import tyrian.Attr
import tyrian.Html

import model.Machine
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanMsg
import spa.plan.PowerOption
import spa.plan.PowerOptions

object PowerOptionsTab:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  private def powerGeneratorControl( env: Env, model: PowerOptions )( generator: Machine ): Html[PowerOption] =
    val isChecked: Boolean = model.allowedGenerators.contains( generator.className )
    Html.div( b.control )(
      Html.label( b.checkbox )(
        Html.input(
          Html.`type` := "checkbox",
          Html.name   := s"power_prefs_${generator.className}",
          Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
          Html.value := isChecked.toString,
          Html.onChange( _ => PowerOption.SetPowerGenerator( generator.className, !isChecked ) )
        ),
        icon.verticalAlign().withDropShadow().machine( env, generator ),
        nbsp,
        Html.text( generator.displayName )
      )
    )

  def apply(
      env: Env,
      model: PowerOptions
  ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Power generators" ) ),
      Html.div( b.panelBlock )(
        Html.div( b.field )( env.powerGenerators.toList.map( powerGeneratorControl( env, model ) ) )
      )
    ).map( _.map( PlanMsg.SetPowerOption( _ ) ) )
