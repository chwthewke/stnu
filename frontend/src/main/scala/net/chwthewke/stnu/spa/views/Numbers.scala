package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import java.text.DecimalFormat

import model.ClockSpeed

object Numbers:
  def showDouble1( d: Double ): String =
    if ( d.isValidInt ) d.toInt.toString
    else f"$d%.1f"

  def showDouble3( d: Double ): String = f"$d%5.3f"

  private val atMost1Decimal: DecimalFormat  = DecimalFormat( "0.#" )
  private val atMost3Decimals: DecimalFormat = DecimalFormat( "0.###" )

  def showDouble1M( d: Double ): String = atMost1Decimal.format( d )
  def showDouble3M( d: Double ): String = atMost3Decimals.format( d )

  def showClockSpeed( cs: ClockSpeed ): String = cs.show
