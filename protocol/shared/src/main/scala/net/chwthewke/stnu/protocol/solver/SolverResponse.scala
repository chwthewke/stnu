package net.chwthewke.stnu
package protocol
package solver

import cats.Show
import cats.data.NonEmptyList
import cats.derived.strict.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import data.Countable
import model.Item
import model.Recipe

sealed trait SolverResponse derives Show, ConfiguredDecoder, ConfiguredEncoder

object SolverResponse:
  case class Solution(
      inputs: Vector[Countable[Double, ClassName[Item]]],
      recipes: Vector[Countable[Double, BoostedRecipe[ClassName[Recipe.NonExtraction]]]]
  ) extends SolverResponse
  case object InvalidModelVersion                                    extends SolverResponse with SolverResponse.Error
  case class InvalidClasses( classes: NonEmptyList[ClassName[Any]] ) extends SolverResponse with SolverResponse.Error
  case class SolverError( message: String )                          extends SolverResponse with SolverResponse.Error

  sealed trait Error extends SolverResponse

  given Decoder[Solution] = ConfiguredDecoder.derived
  given Encoder[Solution] = ConfiguredEncoder.derived

  extension ( response: SolverResponse )
    def solution: Option[SolverResponse.Solution] =
      response match
        case s: SolverResponse.Solution => Some( s )
        case _                          => None
