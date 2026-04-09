package net.chwthewke.stnu
package spa.saved

import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import protocol.solver.SolverRequest
import protocol.solver.SolverResponse
import spa.plan.SolutionModel

object LocalSolution:
  case class Saved(
      request: SolverRequest,
      solution: SolverResponse.Solution
  ) derives ConfiguredEncoder

  object Saved:
    def apply( solutionOpt: Option[SolutionModel] ): Option[Saved] =
      solutionOpt.flatMap: solution =>
        solution.response.solution.map( Saved( solution.requested, _ ) )

  case class Loaded(
      request: SolverRequest,
      solution: SolverResponse.Solution
  ) derives ConfiguredDecoder:
    def toSolutionModel: SolutionModel = SolutionModel( request, solution )
