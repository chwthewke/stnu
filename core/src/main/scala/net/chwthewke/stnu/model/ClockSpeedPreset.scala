package net.chwthewke.stnu
package model

enum ClockSpeedPreset:
  case `100%`
  case `250%`

  def value: ClockSpeed =
    this match
      case ClockSpeedPreset.`100%` => ClockSpeed.ofPercent( 100d )
      case ClockSpeedPreset.`250%` => ClockSpeed.ofPercent( 250d )

object ClockSpeedPreset extends Enum[ClockSpeedPreset] with CirceEnum[ClockSpeedPreset]
