package net.chwthewke.stnu
package spa.prod

import cats.data.NonEmptyVector

import data.Countable
import model.Transport

case class GroupTransport(
    transport: Countable[Double, Transport],
    localEnds: NonEmptyVector[Countable[Double, LocalGroupEnd]],
    localAdjacent: Int,
    remoteEnds: NonEmptyVector[Countable[Double, RemoteGroupEnd]]
) extends FlowTransport:
  override def balanced: Boolean = true // unbalanced warnings are irrelevant
