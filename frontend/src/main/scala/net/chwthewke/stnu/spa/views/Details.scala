package net.chwthewke.stnu
package spa
package views

import tyrian.Attr
import tyrian.CSS
import tyrian.Elem
import tyrian.Html

import spa.css.Bulma
import spa.css.Classes

case class Details[+A](
    isOpen: Boolean,
    toggle: Option[Boolean => A],
    headerClasses: Classes,
    headerAttrs: List[Attr[A]],
    headerLeft: List[Elem[A]],
    headerRight: List[Elem[A]]
):
  private val b: Bulma = Bulma

  def apply[AA >: A]( title: Elem[AA], contents: Elem[AA] ): Html[AA] =
    Html.details( b.card + b.block, Html.open( isOpen ) )(
      Html.summary(
        b.cardHeader
      )(
        Html.div(
          b.cardHeaderTitle + headerClasses,
          Html.style( CSS.display( "inline-block" ) ),
          toggle.map( toMsg => Html.onClick( toMsg( isOpen ) ) )
        )(
          headerLeft ++ ( Html.span( b.isSize5 )( title ) :: headerRight )
        )
      ),
      contents
    )

  def toggleOnClick[AA >: A]( msg: Boolean => AA ): Details[AA] = copy( toggle = Some( msg ) )

  def headerClasses( classes: Classes ): Details[A]              = copy( headerClasses = classes )
  def headerAttrs[AA >: A]( attrs: List[Attr[AA]] ): Details[AA] = copy( headerAttrs = attrs )
  def headerLeft[AA >: A]( elems: List[Elem[AA]] ): Details[AA]  = copy( headerLeft = elems )
  def headerRight[AA >: A]( elems: List[Elem[AA]] ): Details[AA] = copy( headerRight = elems )

object Details:
  def open( isOpen: Boolean ): Details[Nothing]              = Details( isOpen, None, Classes(), Nil, Nil, Nil )
  def apply[A]( title: Elem[A], contents: Elem[A] ): Html[A] = open( isOpen = false ).apply( title, contents )
  def open: Details[Nothing]                                 = open( isOpen = true )
