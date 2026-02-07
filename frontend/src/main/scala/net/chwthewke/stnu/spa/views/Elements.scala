package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.Attr
import tyrian.Elem
import tyrian.Html

import spa.css.Bulma
import spa.css.Classes

object Elements:
  val b: Bulma = Bulma

  def miniButtonWithMod[M]( classes: Classes, title: String, icon: ButtonContent, attrs: Attr[M]* )(
      msg: KeyModifier => M
  ): Html[M] =
    miniButtonGeneric( classes, title, icon, Html.onClickModified( msg ) :: attrs.toList* )

  def miniButton[M]( classes: Classes, title: String, icon: ButtonContent, attrs: Attr[M]* )(
      msg: Option[M]
  ): Html[M] =
    miniButtonGeneric( classes, title, icon, msg.map( Html.onClick ) ++: attrs.toList* )

  private def miniButtonGeneric[M]( classes: Classes, title: String, icon: ButtonContent, attrs: Attr[M]* ): Html[M] =
    Html.button(
      ( b.button + b.isSmall + classes: Attr[Nothing] )
        :: ( Html.title := title )
        :: attrs.toList
    )( icon.element )

  def miniButtonWithDropdown[M]( id: String, classes: Classes, title: String, icon: ButtonContent )(
      content: Html[M]*
  ): Html[M] =
    Html.div( b.dropdown )(
      Html.div( b.dropdownTrigger )(
        Html.button(
          b.button + b.isSmall + classes,
          Html.title := title,
          Html.attribute( "aria-haspopup", "true" ),
          Html.attribute( "aria-controls", id )
        )( icon.element )
      ),
      Html.div( b.dropdownMenu, Html.id := id, Html.role := "menu" )(
        Html.div( b.dropdownContent )(
          content
            .map( el => List( Html.div( b.dropdownItem )( el ) ) )
            .toList
            .intercalate( List( Html.hr( b.dropdownDivider ) ) )
        )
      )
    )

  def messageCenteredHeader[A]( messageClasses: Classes, headerClasses: Classes, header: Elem[A] )(
      content: Elem[A]*
  ): Html[A] =
    Html.p( b.message + messageClasses )(
      Html.div( b.messageHeader + headerClasses )(
        Html.span( b.isFlexGrow1 )(),
        header,
        Html.span( b.isFlexGrow1 )()
      ),
      Html.div( b.messageBody )( content* )
    )

  sealed abstract class ButtonContent( val element: Html[Nothing] )
  object ButtonContent:
    given Conversion[Classes, ButtonContent] = ( cls: Classes ) => new ButtonContent( Html.i( cls )() ) {}
    given Conversion[String, ButtonContent]  = ( txt: String ) => new ButtonContent( Html.span( txt ) ) {}
