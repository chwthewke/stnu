package net.chwthewke.stnu
package spa
package views

import tyrian.Html

import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg

object PlanView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: PlanModel ): Html[PlanMsg] =
    val realEnv: Env = if ( model.recipeOptions.hideFicsmas ) env.withoutFicsmas else env

    Html.div( b.columns )(
      Html.div( b.column + b.isOneQuarter )(
        if ( model.ui.optionsOpen )
          PlanOptionsView.optionsPanel( realEnv, model )
        else
          PlanOptionsView.collapsedOptionsPanel( model )
      )
    )
