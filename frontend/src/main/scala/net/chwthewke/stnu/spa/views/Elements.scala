package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.Attr
import tyrian.CSS
import tyrian.Elem
import tyrian.Html

import model.ClockSpeedPreset
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

  def clockSpeedField[A <: ClockSpeedPreset]( name: String, all: Enum[A], current: A ): Html[A] =
    Html.div( b.field )(
      all.cases.toList.map: cs =>
        Html.div( b.control )(
          Html.label( b.radio )(
            Html.input(
              Html.`type` := "radio",
              Html.name   := name,
              Option.when[Attr[Nothing]]( current == cs )( Html.checked ),
              Html.onChange( _ => cs )
            ),
            nbsp,
            Html.text( cs.toString )
          )
        )
    )

  def resourceMeter( height: Double, unit: String, value: Double ): Html[Nothing] =
    Html.div(
      Html.styles(
        CSS.marginLeft( "auto" ),
        CSS.height( s"$height$unit" ),
        CSS.width( "0.5em" ),
        CSS.borderRadius( "2px" ),
        CSS.backgroundColor( "#CCC" ),
        CSS.position( "relative" )
      )
    )(
      Html.div(
        Html.styles(
          CSS.width( "0.5em" ),
          CSS.height( f"${value * height}%.2f$unit" ),
          CSS.backgroundColor( s"hsl(${( 120d * value ).toInt} 100% 50%)" ),
          CSS.position( "absolute" ),
          CSS.bottom( "0px" )
        )
      )()
    )
