package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine

case class ExtractionOptions(
    minerClass: ClassName[Machine],
    clockSpeed: ClockSpeedPreset,
    extractors: Set[ExtractorType],
    preferFracking: Set[ClassName[Item]],
    resourceWeightSliders: Map[ClassName[Item], Int]
) derives ConfiguredCodec
