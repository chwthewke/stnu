package net.chwthewke.stnu
package spa
package prod

import cats.syntax.all.*

import data.Countable
import model.Transport
import model.prod.FlowEnd

case class ItemTransport(
    transport: Countable[Double, Transport],
    sources: Vector[Countable[Double, Split[SrcDest.Src]]],
    destinations: Vector[Countable[Double, Split[SrcDest.Dest]]]
) extends FlowTransport:
  override def balanced: Boolean =
    ( sources.foldMap( _.amount ) - destinations.foldMap( _.amount ) ).abs < Countable.Tolerance

  def amount: Double = sources.foldMap( _.amount ).max( destinations.foldMap( _.amount ) )

  def get( direction: FlowEnd ): Vector[Countable[Double, Split[SrcDest]]] =
    direction match
      case FlowEnd.Source      => sources
      case FlowEnd.Destination => destinations

object ItemTransport
