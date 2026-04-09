package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine
import spa.plan.ExtractionOptions

object LocalExtractionOptions:
  case class Saved(
      minerClass: ClassName[Machine],
      clockSpeed: String,
      excludeWaterPumpFromOverclocking: Boolean,
      extractors: Set[ExtractorType],
      preferFracking: Set[ClassName[Item]],
      resourceWeightSliders: Map[ClassName[Item], Int]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( extractionOptions: ExtractionOptions ): Saved =
      Saved(
        extractionOptions.minerClass,
        ClockSpeedPreset.keyOf( extractionOptions.clockSpeed ),
        extractionOptions.excludeWaterPumpFromOverclocking,
        extractionOptions.extractors,
        extractionOptions.preferFracking,
        extractionOptions.resourceWeightSliders
      )

  case class Loaded(
      minerClass: ClassName[Machine],
      clockSpeed: String,
      excludeWaterPumpFromOverclocking: Boolean,
      extractors: Set[ExtractorType],
      preferFracking: Set[ClassName[Item]],
      resourceWeightSliders: Map[ClassName[Item], Int]
  ) derives ConfiguredDecoder:
    def toExtractionOptions: ExtractionOptions =
      ExtractionOptions(
        minerClass,
        ClockSpeedPreset.Extraction.withNameOption( clockSpeed ).getOrElse( ClockSpeedPreset.`100%` ),
        excludeWaterPumpFromOverclocking,
        extractors,
        preferFracking,
        resourceWeightSliders
      )
