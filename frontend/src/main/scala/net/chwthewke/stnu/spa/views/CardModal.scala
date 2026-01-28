package net.chwthewke.stnu
package spa
package views

import tyrian.Elem
import tyrian.Html

import spa.css.Bulma
import spa.css.Classes

object CardModal:
  val b: Bulma = Bulma

  def apply[M]( title: Elem[Nothing], closeMsg: M )( body: Html[M] )(
      buttons: List[( Classes, M, Elem[Nothing] )]
  ): Html[M] =
    Html.div( b.modal + b.isActive )(
      Html.div( b.modalBackground )(),
      Html.div( b.modalCard )(
        Html
          .header( b.modalCardHead )(
            Html.p( b.modalCardTitle )( title ),
            Html.button( b.delete, Html.onClick( closeMsg ) )()
          ),
        Html.section( b.modalCardBody )( body ),
        Html.footer( b.modalCardFoot )(
          Html.div( b.buttons )(
            buttons.map:
              case ( classes, msg, content ) =>
                Html.button(
                  b.button + classes,
                  Html.onClick( msg )
                )( content )
          )
        )
      )
    )
