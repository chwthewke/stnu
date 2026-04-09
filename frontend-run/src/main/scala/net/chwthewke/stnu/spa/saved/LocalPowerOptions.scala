package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.ClockSpeedPreset
import model.Machine
import spa.plan.PowerOptions

object LocalPowerOptions:
  case class Saved(
      allowed: Set[ClassName[Machine]],
      maxProductionBoost: Int,
      manufacturingClockSpeed: ClockSpeedPreset
  ) derives ConfiguredEncoder

  object Saved:
    def apply( options: PowerOptions ): Saved =
      Saved( options.allowedGenerators, options.maxProductionBoost, options.manufacturingClockSpeed )

  case class Loaded(
      allowed: Set[ClassName[Machine]],
      maxProductionBoost: Int,
      manufacturingClockSpeed: ClockSpeedPreset
  ) derives ConfiguredDecoder:
    def toPowerOptions: PowerOptions =
      PowerOptions(
        allowed,
        InputModel.withDefault( maxProductionBoost.toString ),
        maxProductionBoost,
        manufacturingClockSpeed
      )
