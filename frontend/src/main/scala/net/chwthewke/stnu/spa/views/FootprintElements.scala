package net.chwthewke.stnu
package spa
package views

import java.text.DecimalFormat
import tyrian.Elem
import tyrian.Html

import model.Footprint

object FootprintElements:
  private val footprintDecimalFormat: DecimalFormat = new DecimalFormat( "##0.###" )

  private def fmtDim( dim: Double ): String = footprintDecimalFormat.format( dim )

  def text( footprint: Footprint ): Elem[Nothing] =
    Html.span(
      Html.strong( fmtDim( footprint.length / 100d ) ),
      Html.text( "m \u00d7 " ),
      Html.strong( fmtDim( footprint.width / 100d ) ),
      Html.text( "m (" ),
      Html.strong( fmtDim( footprint.length / 800d ) ),
      Html.text( " fd. \u00d7 " ),
      Html.strong( fmtDim( footprint.width / 800d ) ),
      Html.text( " fd.)" )
    )
