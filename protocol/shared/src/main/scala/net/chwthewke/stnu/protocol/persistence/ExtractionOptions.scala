package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine

case class ExtractionOptions(
    minerClass: ClassName[Machine],
    clockSpeed: ClockSpeedPreset.Extraction,
    excludeWaterPumpFromOverclocking: Boolean,
    extractors: Set[ExtractorType],
    preferFracking: Set[ClassName[Item]],
    resourceWeightSliders: Map[ClassName[Item], Int]
) derives ConfiguredDecoder,
      ConfiguredEncoder
