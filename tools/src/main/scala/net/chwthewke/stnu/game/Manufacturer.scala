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
    isCollider: Boolean
)

object Manufacturer:
  def manufacturerDecoder( isCollider: Boolean ): Decoder[Manufacturer] =
    given Decoder[Double] = Decoders.doubleStringDecoder
    Decoder.forProduct4(
      "ClassName",
      "mDisplayName",
      "mPowerConsumption",
      "mPowerConsumptionExponent"
    )( ( cn, dn, pc, pe ) => Manufacturer( cn, dn, pc, pe, isCollider ) )

  given Show[Manufacturer] = Show.show: manufacturer =>
    show"""${manufacturer.displayName} # ${manufacturer.className}
          |Power: ${f"${manufacturer.powerConsumption}%.0f"} MW (exp: ${f"${manufacturer.powerConsumptionExponent}%.4f"})
          |""".stripMargin
