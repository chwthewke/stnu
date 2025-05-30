package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*

import model.ClockSpeed

object Numbers:
  def showDouble1( d: Double ): String =
    if ( d.isValidInt ) d.toInt.toString
    else f"$d%.1f"

  def showDouble3( d: Double ): String         = f"$d%5.3f"
  def showClockSpeed( cs: ClockSpeed ): String = cs.show
