package net.chwthewke.stnu

import tyrian.Attr
import tyrian.Html

import css.BulmaClass

package object spa:
  def bc( bulmaClasses: BulmaClass* ): Attr[Nothing] =
    classes( bulmaClasses = bulmaClasses )
  def classes( regularClasses: Seq[String] = Seq(), bulmaClasses: Seq[BulmaClass] = Seq() ): Attr[Nothing] =
    Html.className := ( regularClasses ++ bulmaClasses.map( _.`class` ) ).distinct.mkString( " " )
