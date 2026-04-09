package net.chwthewke.stnu
package spa
package views

import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import model.ClockSpeedPreset
import model.Item
import model.Machine
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanMsg
import spa.plan.PowerOption
import spa.plan.PowerOptions
import spa.plan.RecipeOption

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

  private def maxProductionBoostField( env: Env, model: PowerOptions ): Html[PowerOption] =
    val maxProdBoostId: String = "prod_boost_max"
    Html.div( b.field )(
      Html.label( Html.`for` := maxProdBoostId, b.mr2 )(
        icon.verticalAlign().withDropShadow().item( env, Item.somersloop )
      ),
      Html.span( b.control, Html.id := maxProdBoostId )(
        Html.input(
          Html.`type` := "number",
          Html.min    := "0",
          Html.max    := env.game.defaultResourceOptions.maxProductionBoostShards.toString,
          Html.value  := model.maxProductionBoost.toString,
          Html.style( CSS.width( "4em" ) ),
          Html.onInput( PowerOption.SetMaxProductionBoost( _ ) )
        )
      ),
      Html.span( b.ml2 )( s"(max: ${env.game.defaultResourceOptions.maxProductionBoostShards})" )
    )

  private def productionBoostManufacturingClockSpeedField( model: PowerOptions ): Html[PowerOption] =
    val manufacturingClockSpeedId: String = "prod_boost_clockspeed"
    Elements
      .clockSpeedField( manufacturingClockSpeedId, ClockSpeedPreset, model.manufacturingClockSpeed )
      .map( PowerOption.SetManufacturingClockSpeed( _ ) )

  private def productionBoostBlock( env: Env, model: PowerOptions ): Html[PlanMsg] =
    Html.div(
      Html.div( b.message + b.isWarning )(
        Html.div( b.messageBody )(
          Html.p( """Warning! Computing the optimal recipes with production amplification is extremely expensive
                   |unless the number of possible recipes is limited. It is recommended to set the recipes
                   |to the current recipes before proceeding.""".stripMargin ),
          Html.button(
            b.button + b.isInfo + b.mt2,
            Html.onClick( PlanMsg.SetRecipeOption( RecipeOption.SetCurrent ) )
          )( "Do it!" )
        )
      ),
      Html
        .div( b.panelBlock + b.columns )(
          Html.div( b.column + b.isHalf )( maxProductionBoostField( env, model ) ),
          Html.div( b.column + b.isHalf )(
            Html.label( b.label )( "Max clock speed when amplified" ),
            productionBoostManufacturingClockSpeedField( model )
          )
        )
        .map( PlanMsg.SetPowerOption( _ ) )
    )

  def apply(
      env: Env,
      model: PowerOptions
  ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Power generators" ) ),
      Html.div( b.panelBlock )(
        Html.div( b.field )(
          env.powerGenerators.toList.map( powerGeneratorControl( env, model )( _ ).map( PlanMsg.SetPowerOption( _ ) ) )
        )
      ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Production amplification" ) ),
      productionBoostBlock( env, model )
    )
