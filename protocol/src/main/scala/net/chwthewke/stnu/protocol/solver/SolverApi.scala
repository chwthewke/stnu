package net.chwthewke.stnu
package protocol
package solver

import cats.~>

import protocol.codec.PathCodec
import protocol.codec.UriCodec

trait SolverApi[F[_]]:
  self =>

  def solve( request: SolverRequest ): F[SolverResponse]

  final def mapK[G[_]]( f: F ~> G ): SolverApi[G] =
    new SolverApi[G]:
      override def solve( request: SolverRequest ): G[SolverResponse] =
        f( self.solve( request ) )

object SolverApi:

  import PathCodec.Root

  val postSolverRequest: UriCodec.Constant = Root / "api" / "solve"
