package net.chwthewke.stnu
package game

import cats.Show
import cats.syntax.all.*
import io.circe.Decoder

final case class Manufacturer(
    className: ClassName[Manufacturer],
    displayName: String,
    powerConsumption: Double,
    powerConsumptionExponent: Double,
    productionBoostPowerConsumptionExponent: Double,
    productionShardBoostMultiplier: Double,
    productionShardSlotSize: Int,
    isCollider: Boolean
)

object Manufacturer:
  def manufacturerDecoder( isCollider: Boolean ): Decoder[Manufacturer] =
    given Decoder[Double] = Decoders.doubleStringDecoder
    Decoder.forProduct7(
      "ClassName",
      "mDisplayName",
      "mPowerConsumption",
      "mPowerConsumptionExponent",
      "mProductionBoostPowerConsumptionExponent",
      "mProductionShardBoostMultiplier",
      "mProductionShardSlotSize"
    )( Manufacturer.of( _, _, _, _, _, _, _, isCollider ) )

  private def of(
      cn: ClassName[Manufacturer],
      dn: String,
      powerConsumption: Double,
      powerConsumptionExponent: Double,
      productionBoostPowerConsumptionExponent: Double,
      productionShardBoostMultiplier: Double,
      productionShardSlotSize: Int,
      isCollider: Boolean
  ): Manufacturer =
    // NOTE for some reason the docs incorrectly represent the smelter as having no production amplification slots
    Manufacturer(
      cn,
      dn,
      powerConsumption,
      powerConsumptionExponent,
      productionBoostPowerConsumptionExponent,
      productionShardBoostMultiplier,
      if ( cn.name == "Build_SmelterMk1_C" ) 1 else productionShardSlotSize,
      isCollider
    )

  given Show[Manufacturer] = Show.show: manufacturer =>
    show"""${manufacturer.displayName} # ${manufacturer.className}
          |Power: ${f"${manufacturer.powerConsumption}%.0f"} MW (exp: ${f"${manufacturer.powerConsumptionExponent}%.4f"})
          |""".stripMargin
