package net.chwthewke.stnu
package model

import cats.Show
import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class Machine(
    className: ClassName[Machine],
    displayName: String,
    machineType: MachineType,
    powerConsumption: Double,
    powerConsumptionExponent: Double
) derives ConfiguredDecoder,
      ConfiguredEncoder

object Machine:
  given Show[Machine] = Show.show:
    case Machine( className, displayName, machineType, powerConsumption, powerConsumptionExponent ) =>
      show"""$displayName # $className
            |$machineType
            |Power: ${f"$powerConsumption%.0f MW"} (exp: ${f"$powerConsumptionExponent%.4f"})""".stripMargin
