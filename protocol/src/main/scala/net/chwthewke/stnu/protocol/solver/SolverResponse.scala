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

enum SolverResponse derives Show, ConfiguredDecoder, ConfiguredEncoder:
  case Solution(
      inputs: Vector[Countable[Double, ClassName[Item]]],
      recipes: Vector[Countable[Double, ClassName[Recipe]]]
  )                                                            extends SolverResponse with SolverResponse.Ok_
  case InvalidModelVersion                                     extends SolverResponse with SolverResponse.Error_
  case InvalidClasses( classes: NonEmptyList[ClassName[Any]] ) extends SolverResponse with SolverResponse.Error_
  case SolverError( message: String )                          extends SolverResponse with SolverResponse.Error_

object SolverResponse:
  sealed trait Ok_
  sealed trait Error_

  type Ok    = SolverResponse & SolverResponse.Ok_
  type Error = SolverResponse & SolverResponse.Error_
