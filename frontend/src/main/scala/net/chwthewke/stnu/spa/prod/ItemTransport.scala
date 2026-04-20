package net.chwthewke.stnu
package spa
package prod

import cats.syntax.all.*

import data.Countable
import model.Transport
import model.prod.FlowEnd
import model.prod.Group

case class ItemTransport(
    transport: Transport,
    sourceAmount: Double,
    destinationAmount: Double,
    sources: Vector[Countable[Double, ItemTransport.Peer[SrcDest.Src]]],
    destinations: Vector[Countable[Double, ItemTransport.Peer[SrcDest.Dest]]]
) extends FlowTransport:

  override val amount: Double = sourceAmount.max( destinationAmount )

  override def balanced: Boolean =
    ( sourceAmount - destinationAmount ).abs < Countable.Tolerance

  def getPeers( direction: FlowEnd ): Vector[Countable[Double, ItemTransport.Peer[SrcDest]]] =
    direction match
      case FlowEnd.Source      => sources
      case FlowEnd.Destination => destinations

  def getSplitPeers( direction: FlowEnd ): Vector[Countable[Double, Split[SrcDest]]] =
    direction match
      case FlowEnd.Source      => sources.mapFilter( _.traverse( _.split ) )
      case FlowEnd.Destination => destinations.mapFilter( _.traverse( _.split ) )

  def getTransportPeers( direction: FlowEnd ): Vector[ItemTransport.TransportPeer] =
    getPeers( direction ).collect {
      case Countable( t @ ItemTransport.Peer.From( _, _ ), _ ) => t
      case Countable( t @ ItemTransport.Peer.To( _, _ ), _ )   => t
    }

object ItemTransport:
  sealed trait TransportPeer:
    def index: Int

  enum Peer[+A <: SrcDest]:
    case End( ofSplit: Split[A] )                 extends Peer[A]
    case From( transport: Transport, index: Int ) extends Peer[SrcDest.Src] with TransportPeer  // 0-based
    case To( transport: Transport, index: Int )   extends Peer[SrcDest.Dest] with TransportPeer // 0-based

    def split: Option[Split[A]] = this match
      case Peer.End( s )     => s.some
      case Peer.From( _, _ ) => none
      case Peer.To( _, _ )   => none

    def splitNumber: Option[( Int, Int )] = split.map( s => ( s.number, s.max ) )
    def group: Option[Group]              = split.map( _.group )

    def transportPeer: Option[TransportPeer] = this match
      case _: Peer.End[_] => none
      case p: Peer.From   => p.some
      case p: Peer.To     => p.some

    def isTransportSplit: Boolean = transportPeer.isDefined

  def apply(
      transport: Transport,
      sources: Vector[Countable[Double, ItemTransport.Peer[SrcDest.Src]]],
      destinations: Vector[Countable[Double, ItemTransport.Peer[SrcDest.Dest]]]
  ): ItemTransport =
    val srcAmt: Double  = sources.foldMap( _.amount )
    val destAmt: Double = destinations.foldMap( _.amount )
    ItemTransport( transport, srcAmt, destAmt, sources, destinations )
