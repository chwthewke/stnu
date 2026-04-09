package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.ClockSpeedPreset
import model.Machine

case class PowerOptions(
    allowedGenerators: Set[ClassName[Machine]],
    maxProductionBoost: Int,
    manufactutingClockSpeed: ClockSpeedPreset
) derives ConfiguredCodec
