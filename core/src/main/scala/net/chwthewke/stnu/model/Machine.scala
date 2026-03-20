package net.chwthewke.stnu
package model

import cats.Order
import cats.Show
import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class Machine(
    className: ClassName[Machine],
    displayName: String,
    machineType: MachineType,
    tier: Tier,
    powerConsumption: Double,
    powerConsumptionExponent: Double,
    footprint: Option[Footprint]
) derives ConfiguredDecoder,
      ConfiguredEncoder

object Machine:
  given Show[Machine] = Show.show:
    case Machine( className, displayName, machineType, tier, powerConsumption, powerConsumptionExponent, footprint ) =>
      show"""$displayName # $className
            |$machineType Tier $tier
            |Power: ${f"$powerConsumption%.0f MW"} (exp: ${f"$powerConsumptionExponent%.4f"})
            |Footprint: ${footprint.fold( "-" )( _.show )}
            |""".stripMargin

  given Order[Machine]    = Order.by( _.className )
  given Ordering[Machine] = Order.catsKernelOrderingForOrder
