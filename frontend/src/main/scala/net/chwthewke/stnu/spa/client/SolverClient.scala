package net.chwthewke.stnu
package spa
package client

import cats.data.Kleisli
import cats.effect.Async
import org.http4s.Method.POST
import org.http4s.client.Client

import protocol.solver.SolverApi
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

class SolverClient[F[_]: Async] extends SolverApi[[a] =>> Kleisli[F, Client[F], a]] with CirceClient[F]:
  override def solve( request: SolverRequest ): Kleisli[F, Client[F], SolverResponse] =
    expect[SolverResponse]( POST( request, SolverApi.postSolverRequest() ) )
