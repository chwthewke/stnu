package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.Machine
import spa.plan.PowerOptions

object LocalPowerOptions:
  case class Saved( allowed: Set[ClassName[Machine]] ) derives ConfiguredEncoder

  object Saved:
    def apply( options: PowerOptions ): Saved = Saved( options.allowedGenerators )

  case class Loaded( allowed: Set[ClassName[Machine]] ) derives ConfiguredDecoder:
    def toPowerOptions: PowerOptions = PowerOptions( allowed )
