package net.chwthewke.stnu
package spa

import tyrian.Attr
import tyrian.Elem
import tyrian.Empty
import tyrian.EmptyAttribute
import tyrian.Html

import css.CssClass
import css.Classes

package object views:
  val nbsp: Elem[Nothing] = Html.raw( "span" )( "&nbsp;" )

  def classes( classes: CssClass* ): Attr[Nothing] =
    Html.className := classes.distinct.mkString( " " )

  extension ( bc: CssClass )
    def +( c: CssClass ): Classes         = Classes( Vector( bc, c ) )
    def +( c: Option[CssClass] ): Classes = Classes( Vector( bc ) ++ c )

  extension ( bc: Option[CssClass] )
    def +( c: CssClass ): Classes         = Classes( bc ++: Vector( c ) )
    def +( c: Option[CssClass] ): Classes = Classes( bc.toVector ++ c )

  given Conversion[Option[CssClass], Attr[Nothing]] = c => c.fold( EmptyAttribute )( c => classes( c ) )
  given Conversion[CssClass, Attr[Nothing]]         = c => classes( c )
  given Conversion[Classes, Attr[Nothing]]          = c => classes( c.classes* )

  given convertAttrOption[A]: Conversion[Option[Attr[A]], Attr[A]] = _.fold[Attr[A]]( EmptyAttribute )( identity )
  given convertElemOption[A]: Conversion[Option[Elem[A]], Elem[A]] = _.fold[Elem[A]]( Empty )( identity )
