package net.chwthewke.stnu

import tyrian.Attr
import tyrian.Html

import spa.css.CssClass

package object spa:
  def bc( bulmaClasses: CssClass* ): Attr[Nothing] =
    classes( bulmaClasses = bulmaClasses )
  def classes( regularClasses: Seq[String] = Seq(), bulmaClasses: Seq[CssClass] = Seq() ): Attr[Nothing] =
    Html.className := ( regularClasses ++ bulmaClasses.map( _.`class` ) ).distinct.mkString( " " )
