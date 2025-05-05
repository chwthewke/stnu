package net.chwthewke.stnu
package spa.views

object Numbers:
  def showDouble1( d: Double ): String =
    if ( d == d.floor ) d.toInt.toString
    else f"$d%.1f"
