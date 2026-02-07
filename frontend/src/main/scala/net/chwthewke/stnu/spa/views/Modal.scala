package net.chwthewke.stnu
package spa
package views

import tyrian.Attr
import tyrian.Elem
import tyrian.Html

import spa.css.Bulma
import spa.css.Classes

object Modal:
  val b: Bulma = Bulma

  def apply[M]( closeMsg: M, attrs: Attr[M]* )( body: Html[M] ): Html[M] =
    Html.div( b.modal + b.isActive )(
      Html.div( b.modalBackground, Html.onClick( closeMsg ) )(),
      Html.div( ( b.modalContent: Attr[Nothing] ) :: attrs.toList )( body ),
      Html.button( b.modalClose + b.isLarge, Html.onClick( closeMsg ) )()
    )

  def card[M]( title: Elem[Nothing], closeMsg: M )( body: Html[M] )(
      buttons: List[( Classes, M, Elem[Nothing] )]
  ): Html[M] =
    Html.div( b.modal + b.isActive )(
      Html.div( b.modalBackground, Html.onClick( closeMsg ) )(),
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
