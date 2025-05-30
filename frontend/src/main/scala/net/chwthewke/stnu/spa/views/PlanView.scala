package net.chwthewke.stnu
package spa
package views

import tyrian.CSS
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

  private def mainPlanContent( env: Env, model: PlanModel ): List[Html[PlanMsg]] =
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
      List(
        Html.section( b.hero + b.isPrimary )(
          Html.div( b.heroBody )(
            Html.p( b.title )( "Factory plan" ),
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
