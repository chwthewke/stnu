package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyVector
import cats.syntax.all.*

import data.Countable
import model.Item
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.ProcessSplitId

object Shown:
  def showSplit( split: ( ProcessSplitId, ( Double, Group ) ), ix: Int ): String =
    s"- ${ix + 1} #${split._1} [${split._2._2}] ${split._2._1}"

  def showSplits( kv: ( EndId, ProcessSplits ) ): String =
    s"""  ${kv._1}
       |    ${kv._2.splits.toVector.zipWithIndex.map( showSplit ).mkString_( "\n    " )}""".stripMargin

  def showSrcDest( sd: SrcDest ): String = sd match
    case SrcDest.Extract( process ) => f"${process.fractionalAmount}%.3f ${process.recipe.className}%s"
    case SrcDest.Step( process )    => f"${process.fractionalAmount}%.3f ${process.recipe.className}%s"
    case SrcDest.Input              => "INPUT"
    case SrcDest.Requested          => "REQUEST"
    case SrcDest.Byproduct          => "BYPRODUCT"

  def showSplitSrcDest( split: Split[SrcDest] ): String =
    val splitNumber = if ( split.max > 1 ) s" ${split.number}/${split.max}" else ""
    showSrcDest( split.value ) + splitNumber

  def showItemTransportPeer( peer: ItemTransport.Peer[SrcDest] ): String =
    peer match
      case ItemTransport.Peer.End( split )             => showSplitSrcDest( split )
      case ItemTransport.Peer.From( transport, index ) => s"FROM ${transport.displayName} ${index + 1}"
      case ItemTransport.Peer.To( transport, index )   => s"TO ${transport.displayName} ${index + 1}"

  def showItemTransportPeers( peers: Vector[Countable[Double, ItemTransport.Peer[SrcDest]]], arrow: String ): String =
    peers
      .map( cs => f"${cs.amount}%.3f $arrow ${showItemTransportPeer( cs.item )}" )
      .mkString( "\n      " )

  def showItemTransport( itemTransport: ItemTransport, index: Int ): String =
    f"""  - ${itemTransport.transportAmount.amount}%.3f ${itemTransport.transport.className} #$index
       |    FROM
       |      ${showItemTransportPeers( itemTransport.sources, "<-" )}
       |    TO
       |      ${showItemTransportPeers( itemTransport.destinations, "->" )}""".stripMargin

  def showItemFlow( kv: ( ClassName[Item], NonEmptyVector[ItemTransport] ) ): String =
    s"""  ${kv._1}
       |${kv._2.zipWithIndex.map { case ( t, i ) => showItemTransport( t, i + 1 ) }.mkString_( "\n" )}""".stripMargin

  def showRefEnd( endProcesses: Option[NonEmptyVector[ProcessSplitId]] ): String =
    endProcesses
      .foldMap( _.toVector )
      .map( id => show"#$id" )
      .mkString_( "\n      " )

  def showItemTransportRef( itemTransport: ItemTransportRef ): String =
    s"""  - FROM
       |      ${showRefEnd( itemTransport.ends.get( FlowEnd.Source ) )}
       |    TO
       |      ${showRefEnd( itemTransport.ends.get( FlowEnd.Destination ) )}""".stripMargin

  def showTransportSplit( transportSplit: TransportSplit ): String =
    f"  + ${transportSplit.from + 1}  -(${transportSplit.amount}%.3f)-> #${transportSplit.to + 1}"

  def showItemFlowRef( kv: ( ClassName[Item], ItemFlows ) ): String =
    s"""  ${kv._1}
       |${kv._2.transports.map( showItemTransportRef ).mkString_( "\n" )}
       |${kv._2.transportSplits.map( showTransportSplit ).mkString_( "\n" )}""".stripMargin

  def showEndSplit( kv: ( ProcessSplitId, ( Double, Group, EndId ) ) ): String =
    show"  #${kv._1}: ${kv._2._3} x ${kv._2._1} [${kv._2._2}]"
