package net.chwthewke.stnu
package protocol
package solver

import cats.Show
import cats.derived.strict.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import data.Countable
import model.Item
import model.Recipe

case class SolverRequest(
    modelVersion: ModelVersionId,
    requested: Vector[Countable[Double, ClassName[Item]]],
    recipeSelection: Vector[ClassName[Recipe]],
    resources: Map[ClassName[Item], SolverRequest.Resource]
) derives Show,
      ConfiguredDecoder,
      ConfiguredEncoder

object SolverRequest:
  case class Resource( cap: Double, weight: Double ) derives Show, ConfiguredDecoder, ConfiguredEncoder
