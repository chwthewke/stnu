package net.chwthewke.stnu
package protocol
package solver

import cats.Show
import cats.derived.strict.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import data.Countable
import model.ClockSpeedPreset
import model.Item
import model.Recipe
import model.Transport

case class SolverRequest(
    modelVersion: ModelVersionId,
    requested: Vector[Countable[Double, ClassName[Item]]],
    recipeSelection: Set[ClassName[Recipe.NonExtraction]],
    resources: Map[ClassName[Item], SolverRequest.Resource],
    bestConveyorBelt: ClassName[Transport],
    bestPipeline: ClassName[Transport],
    maxProductionBoost: Int,
    manufacturingClockSpeed: ClockSpeedPreset
) derives Show,
      ConfiguredDecoder,
      ConfiguredEncoder

object SolverRequest:
  case class Resource( cap: Option[Double], weight: Double ) derives Show, ConfiguredDecoder, ConfiguredEncoder
