package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle
import tyrian.Html

import data.Countable
import protocol.persistence.PlanSummary
import spa.css.Bulma
import spa.css.Phosphor
import spa.library.LibraryModel
import spa.library.LibraryMsg
import spa.plan.PlanModel

object LibraryView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  private val dtf: DateTimeFormatter = DateTimeFormatter.ofLocalizedDateTime( FormatStyle.MEDIUM )

  def apply( model: LibraryModel, planUi: PlanModel.Ui ): Html[LibraryMsg] =
    model.plans.fold( loading )( libraryTable( model, planUi ) )

  val loading: Html[LibraryMsg] = Html.section( b.container + b.section + b.isLarge )(
    Html.progress( b.progress + b.isMedium + b.isPrimary, Html.max := "100" )( "Loading..." )
  )

  def confirmDeleteModal( plan: PlanSummary ): Html[LibraryMsg] =
    Modal.card(
      Html.text( "Confirm delete?" ),
      LibraryMsg.CloseDeletePlan
    )(
      Html.div(
        Html.p(
          Html.text( show"Are you sure you want to delete plan \"" ),
          Html.em( plan.name.show ),
          Html.text( "\"?" )
        ),
        Html.p( Html.strong( "This cannot be undone." ) )
      )
    )(
      List(
        ( b.isDanger, LibraryMsg.ConfirmDeletePlan( plan.planId ), Html.text( "Delete" ) ),
        ( None, LibraryMsg.CloseDeletePlan, Html.text( "Cancel" ) )
      )
    )

  def libraryTable( model: LibraryModel, planUi: PlanModel.Ui )( plans: Vector[PlanSummary] ): Html[LibraryMsg] =
    Html.div( b.container )(
      model.confirmDeletePlan.map( confirmDeleteModal ),
      Html.table( b.table + b.isFullwidth )(
        Html.thead(
          Html.tr(
            Html.th( "Name" ),
            Html.th( "Contents" ),
            Html.th( "Date" ),
            Html.th( "Delete?" )
          )
        ),
        Html.tbody(
          plans.toList.map( plan =>
            Html.tr(
              Html.td(
                Html.a(
                  Html.href := LocationModel.Plan( planUi.options, plan.planId.some ).toInternalLocation
                )(
                  plan.name.show
                )
              ),
              Html.td(
                Html.div( b.tags + b.mb0 )(
                  plan.requested.toList.map:
                    case ( item, amt ) =>
                      RecipeFrag.numberedIconTag( model.env, Countable( item, amt ) )
                )
              ),
              Html.td(
                dtf.format( plan.updated.atZone( ZoneId.systemDefault ).toLocalDateTime )
              ),
              Html.td(
                Html.button(
                  b.button + b.isSmall + b.isDanger,
                  Html.onClick( LibraryMsg.RequestDeletePlan( plan ) )
                )(
                  Html.i( p.regular.trash )()
                )
              )
            )
          )
        )
      )
    )
