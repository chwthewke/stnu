package net.chwthewke.stnu
package spa
package prod

enum ClockSpeedPreset:
  case `100%`
  case `250%`

object ClockSpeedPreset extends Enum[ClockSpeedPreset]:
  extension ( preset: ClockSpeedPreset )
    def value: ClockSpeed =
      preset match
        case ClockSpeedPreset.`100%` => ClockSpeed( 100d )
        case ClockSpeedPreset.`250%` => ClockSpeed( 250d )
