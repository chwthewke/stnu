package net.chwthewke.stnu
package spa.plan

import cats.data.NonEmptyList

import model.Item
import model.Transport
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

case class SolutionModel(
    requested: SolverRequest,
    otherInputs: SolutionModel.OtherInputs,
    response: SolverResponse
)

object SolutionModel:
  case class OtherInputs(
      preferFracking: Set[ClassName[Item]],
      belts: NonEmptyList[Transport],
      pipelines: NonEmptyList[Transport]
  )
