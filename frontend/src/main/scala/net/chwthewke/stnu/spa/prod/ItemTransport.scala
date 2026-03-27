package net.chwthewke.stnu
package spa
package prod

import cats.syntax.all.*

import data.Countable
import model.Item
import model.Transport
import model.prod.FlowEnd

case class ItemTransport(
    transport: Transport,
    sourceAmount: Double,
    destinationAmount: Double,
    sources: Vector[Countable[Double, Split[SrcDest.Src]]],
    destinations: Vector[Countable[Double, Split[SrcDest.Dest]]],
    transportSplits: Map[FlowEnd, Vector[Countable[Double, Int]]]
) extends FlowTransport:

  override val amount: Double = sourceAmount.max( destinationAmount )

  override def balanced: Boolean =
    ( sourceAmount - destinationAmount ).abs < Countable.Tolerance

  def getMachineFlows( direction: FlowEnd ): Vector[Countable[Double, Split[SrcDest]]] =
    direction match
      case FlowEnd.Source      => sources
      case FlowEnd.Destination => destinations

  def getTransportSplits( direction: FlowEnd ): Vector[Countable[Double, Int]] =
    transportSplits.get( direction ).orEmpty

object ItemTransport:
  private def sourceAmount(
      sources: Vector[Countable[Double, Split[SrcDest.Src]]],
      transportSplits: Map[FlowEnd, Vector[Countable[Double, Int]]]
  ): Double =
    sources.foldMap( _.amount ) + transportSplits.get( FlowEnd.Source ).foldMap( _.foldMap( _.amount ) )

  private def destinationAmount(
      destinations: Vector[Countable[Double, Split[SrcDest.Dest]]],
      transportSplits: Map[FlowEnd, Vector[Countable[Double, Int]]]
  ): Double =
    destinations.foldMap( _.amount ) + transportSplits.get( FlowEnd.Destination ).foldMap( _.foldMap( _.amount ) )

  def apply(
      prod: ProdModel,
      item: Item,
      sources: Vector[Countable[Double, Split[SrcDest.Src]]],
      destinations: Vector[Countable[Double, Split[SrcDest.Dest]]],
      transportSplits: Map[FlowEnd, Vector[Countable[Double, Int]]]
  ): ItemTransport =
    val srcAmt  = sourceAmount( sources, transportSplits )
    val destAmt = destinationAmount( destinations, transportSplits )

    val transport: Transport = prod.selectTransport( item, srcAmt.max( destAmt ) ).item
    ItemTransport( transport, srcAmt, destAmt, sources, destinations, transportSplits )
