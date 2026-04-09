package net.chwthewke.stnu
package model

enum ClockSpeedPreset:
  case `100%` extends ClockSpeedPreset with ClockSpeedPreset.Extraction_
  case `150%` extends ClockSpeedPreset
  case `200%` extends ClockSpeedPreset
  case `250%` extends ClockSpeedPreset with ClockSpeedPreset.Extraction_

  def value: ClockSpeed =
    this match
      case ClockSpeedPreset.`100%` => ClockSpeed.ofPercent( 100d )
      case ClockSpeedPreset.`150%` => ClockSpeed.ofPercent( 150d )
      case ClockSpeedPreset.`200%` => ClockSpeed.ofPercent( 200d )
      case ClockSpeedPreset.`250%` => ClockSpeed.ofPercent( 250d )

object ClockSpeedPreset extends Enum[ClockSpeedPreset] with CatsEnum[ClockSpeedPreset] with CirceEnum[ClockSpeedPreset]:
  sealed trait Extraction_

  type Extraction = ClockSpeedPreset & Extraction_
  object Extraction_ extends Enum[Extraction] with CatsEnum[Extraction] with CirceEnum[Extraction]:
    override def values: Array[Extraction] = Array( `100%`, `250%` )
  val Extraction: Extraction_.type = Extraction_
