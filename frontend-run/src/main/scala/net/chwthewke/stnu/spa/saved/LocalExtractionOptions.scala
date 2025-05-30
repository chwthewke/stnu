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
      extractors: Set[ExtractorType],
      preferFracking: Set[ClassName[Item]],
      resourceWeightSliders: Map[ClassName[Item], Int]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( extractionOptions: ExtractionOptions ): Saved =
      Saved(
        extractionOptions.minerClass,
        ClockSpeedPreset.keyOf( extractionOptions.clockSpeed ),
        extractionOptions.extractors,
        extractionOptions.preferFracking,
        extractionOptions.resourceWeightSliders
      )

  case class Loaded(
      minerClass: ClassName[Machine],
      clockSpeed: String,
      extractors: Set[ExtractorType],
      preferFracking: Set[ClassName[Item]],
      resourceWeightSliders: Map[ClassName[Item], Int]
  ) derives ConfiguredDecoder:
    def toExtractionOptions: ExtractionOptions =
      ExtractionOptions(
        minerClass,
        ClockSpeedPreset.withNameOption( clockSpeed ).getOrElse( ClockSpeedPreset.`100%` ),
        extractors,
        preferFracking,
        resourceWeightSliders
      )
