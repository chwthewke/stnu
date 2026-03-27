package net.chwthewke.stnu
package spa.prod

import cats.data.NonEmptyVector
import cats.syntax.all.*

import data.Countable
import model.Transport

case class GroupTransport(
    transport: Transport,
    localEnds: NonEmptyVector[Countable[Double, LocalGroupEnd]],
    localAdjacent: Int,
    remoteEnds: NonEmptyVector[Countable[Double, RemoteGroupEnd]]
) extends FlowTransport:
  override def amount: Double    = localEnds.foldMap( _.amount )
  override def balanced: Boolean = true // unbalanced warnings are irrelevant
