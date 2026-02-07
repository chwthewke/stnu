package net.chwthewke.stnu
package spa
package views

import tyrian.Attribute
import tyrian.CSS
import tyrian.Html

import model.Form
import model.Item
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.RequestsAction
import spa.plan.RequestsModel

object PlanRequestsView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def requestsButton( model: PlanModel ): Html[Nothing] =
    Html.a( b.button + b.isPrimary, Html.href := model.locationToOpenRequests.toInternalLocation )(
      Html.span( b.iconText )( Html.span( "Requests" ), Html.span( b.icon )( Html.i( p.regular.arrowsOut )() ) )
    )

  def isProjectPart( item: Item ): Boolean =
    item.className.name.startsWith( "Desc_SpaceElevatorPart_" )

  def requestsPanel( model: PlanModel ): Html[PlanMsg] =
    val requests: RequestsModel = model.requests

    val header: Html[Nothing] =
      Html.div( b.panelHeading + b.p2, Html.style( CSS.display( "flex" ) ) )(
        Html.span( Html.style( CSS.flexGrow( "1" ) ) )( "Requests" ),
        Html.a( b.hasTextPrimaryDark, Html.href := model.locationToCloseRequests.toInternalLocation )(
          Html.i( p.regular.arrowsIn )()
        )
      )

    val description: Html[Nothing] =
      val total: Int    = requests.requestAmountEditors.length
      val project: Int  = requests.requestAmountEditors.count( r => isProjectPart( r._1 ) )
      val fluids: Int   = requests.requestAmountEditors.count( r => r._1.form != Form.Solid )
      val storable: Int = total - project - fluids

      def s( n: Int ): String = if ( n == 1 ) "" else "s"

      Html.div(
        Html.p(
          Html.text( "Requesting" ),
          nbsp,
          Html.strong( total.toString ),
          nbsp,
          Html.text( s"part${s( total )}" )
        ),
        Html.p(
          Html.strong( storable.toString ),
          nbsp,
          Html.text( "storable, " ),
          Html.strong( project.toString ),
          nbsp,
          Html.text( s"project part${s( project )}, " ),
          Html.strong( fluids.toString ),
          nbsp,
          Html.text( s"fluid${s( fluids )}" )
        )
      )

    val requestAmountEditors: List[Html[PlanMsg]] =
      requests.requestAmountEditors
        .sortBy( _._1.displayName )
        .map:
          case ( item, editor ) =>
            Html
              .div( b.field + b.hasAddons )(
                Html.div( b.control )(
                  Html
                    .div(
                      b.button + b.isSmall + b.isDanger,
                      Html.style( CSS.height( "var(--bulma-control-height)" ) ),
                      Html.onClick( RequestsAction.Delete( item.className ) )
                    )(
                      Html.i( p.regular.`trash` )()
                    )
                ),
                Html.div( b.control )(
                  Html.input(
                    b.input + b.isSmall,
                    Html.style( CSS.width( "5em" ) ),
                    Html.id     := RequestsModel.editorId( item.className ),
                    Html.`type` := "text",
                    Attribute( "inputmode", "decimal" ),
                    editor.output.map( v => Html.value := v ),
                    Html.onInput( value => RequestsAction.SetAmountValue( item.className, value ) )
                  )
                ),
                Html.label( Html.`for` := RequestsModel.editorId( item.className ), b.ml2 )(
                  icon.verticalAlign().item( model.env, item ),
                  nbsp,
                  Html.text( item.displayName )
                )
              )
              .map( PlanMsg.Requests( _ ) )

    val selectButton: Html[PlanMsg] =
      Html.div( b.field )(
        Html.div( b.control )(
          Html
            .button(
              b.button + b.isSuccess,
              Html.onClick( PlanMsg.ToggleRequestSelection( enable = true ) ),
              Html.disabled( model.ui.isOrganizer )
            )(
              Html.span( b.iconText )(
                Html.span( b.icon )( Html.i( p.bold.`plusSquare` )() ),
                Html.span( "Add request" )
              )
            )
        )
      )

    Html.div( b.panel + b.isPrimary )(
      header,
      Html.div( b.panelBlock )(
        description,
        Html.div( b.isFlexGrow1 )(),
        selectButton
      ),
      Html.div( b.panelBlock )(
        Html.div( b.px2 )( requestAmountEditors )
      )
    )
