package net.chwthewke.stnu
package spa
package prod

import cats.Show
import cats.data.NonEmptyVector
import cats.syntax.all.*
import mouse.option._

import data.Countable
import model.Item
import model.Transport
import model.prod.FlowEnd
import protocol.persistence.ProcessSplitId

case class SrcDestPos( item: Item, direction: FlowEnd, index: Int, subIndex: Int ):

  def getSplitId( itemFlowRefs: Map[ClassName[Item], ItemFlows] ): Option[ProcessSplitId] =
    itemFlowRefs
      .get( item.className )
      .flatMap( _.transports.get( index ) )
      .flatMap( _.ends.get( direction ) )
      .flatMap( _.get( subIndex ) )

  def getSplit(
      itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]]
  ): Option[Countable[Double, Split[SrcDest]]] =
    itemFlows
      .get( item.className )
      .flatMap( _.get( index ) )
      .map( _.getSplitPeers( direction ) )
      .flatMap( _.lift( subIndex ) )

  def getTransportCount( itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]] ): Option[Int] =
    itemFlows.get( item.className ).map( _.length )

  def getTransport(
      itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]]
  ): Option[Transport] =
    itemFlows
      .get( item.className )
      .flatMap( _.get( index ) )
      .map( _.transport )

  def getLocal( itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]] ): Option[ItemTransport] =
    itemFlows
      .get( item.className )
      .flatMap( _.get( index ) )

  def getTransports( itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]] ): Vector[ItemTransport] =
    itemFlows
      .get( item.className )
      .cata( _.toVector, Vector.empty )

  def getOppositeSplits(
      itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]]
  ): List[Countable[Double, ItemTransport.Peer[SrcDest]]] =
    itemFlows.get( item.className ).foldMap( _.foldMap( it => it.getPeers( direction.opposite ).toList ) )

  def getAdjacentSplits(
      itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]]
  ): List[List[Countable[Double, Split[SrcDest]]]] =
    itemFlows.get( item.className ).foldMap( _.map( it => it.getSplitPeers( direction ).toList ).toList )

  override def toString: String = show"${item.displayName}/$index/$direction/$subIndex"

object SrcDestPos:
  given Show[SrcDestPos] = Show.fromToString
