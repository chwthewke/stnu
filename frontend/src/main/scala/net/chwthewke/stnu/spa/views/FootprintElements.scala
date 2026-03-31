package net.chwthewke.stnu
package spa
package views

import tyrian.Elem
import tyrian.Html

import model.Footprint

object FootprintElements:

  def text( footprint: Footprint ): Elem[Nothing] =
    Html.span(
      Html.strong( Numbers.showDouble3M( footprint.length / 100d ) ),
      Html.text( "m \u00d7 " ),
      Html.strong( Numbers.showDouble3M( footprint.width / 100d ) ),
      Html.text( "m (" ),
      Html.strong( Numbers.showDouble3M( footprint.length / 800d ) ),
      Html.text( " fd. \u00d7 " ),
      Html.strong( Numbers.showDouble3M( footprint.width / 800d ) ),
      Html.text( " fd.)" )
    )
