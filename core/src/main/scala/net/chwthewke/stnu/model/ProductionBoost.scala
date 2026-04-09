package net.chwthewke.stnu
package model

import cats.Show
import cats.syntax.all.*
import io.circe.derivation.ConfiguredCodec

case class ProductionBoost(
    slots: Int,
    effect: Double,
    powerConsumptionExponent: Double
) derives ConfiguredCodec:
  def powerConsumptionFactor( used: Int ): Double =
    math.pow( 1d + ( effect * used ), powerConsumptionExponent )

object ProductionBoost:
  given Show[ProductionBoost] = Show.show: boost =>
    f"${boost.slots} slots +${boost.effect * 100}%f (power exp: ${boost.powerConsumptionExponent}%f)"

  extension ( self: Option[ProductionBoost] )
    def effect( usedSlots: Int ): Double           = self.fold( 0d )( _.effect * usedSlots )
    def powerConsumption( usedSlots: Int ): Double =
      self.fold( 1d )( pb => math.pow( 1d + pb.effect * usedSlots, pb.powerConsumptionExponent ) )
