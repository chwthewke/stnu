package net.chwthewke.stnu
package spa.prod

import cats.data.NonEmptyVector
import cats.syntax.all.*
import mouse.option.*
import scala.collection.immutable.SortedMap

import data.Countable
import model.Item
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.ProcessSplitId

case class ShownFlows( self: Flows ):

  private def showSplit( split: ( ProcessSplitId, ( Double, Group ) ), ix: Int ): String =
    s"- ${ix + 1} #${split._1} [${split._2._2}] ${split._2._1}"

  private def showSplits( kv: ( EndId, ProcessSplits ) ): String =
    s"""  ${kv._1}
       |    ${kv._2.splits.toVector.zipWithIndex.map( showSplit ).mkString_( "\n    " )}""".stripMargin

  private def showSrcDest( srcDest: SrcDest ): String =
    srcDest match
      case SrcDest.Extract( process ) =>
        f"${process.fractionalAmount}%.3f ${process.recipe.className}%s"
      case SrcDest.Step( process ) =>
        f"${process.fractionalAmount}%.3f ${process.recipe.className}%s"
      case SrcDest.Input     => "INPUT"
      case SrcDest.Requested => "REQUESTED"
      case SrcDest.Byproduct => "BYPRODUCT"

  private def showSplitSrcDest( split: Split[SrcDest] ): String =
    val splitNumber = if ( split.max > 1 ) s" ${split.split}/${split.max}" else ""
    showSrcDest( split.value ) + splitNumber

  private def showSrcDests( srcDests: Vector[Countable[Double, Split[SrcDest]]], arrow: String ): String =
    srcDests
      .map( cs => f"${cs.amount}%.3f $arrow ${showSplitSrcDest( cs.item )}" )
      .mkString( "\n      " )

  private def showItemTransport( itemTransport: ItemTransport ): String =
    f"""  - ${itemTransport.transportAmount.amount}%.3f ${itemTransport.transport.className}
       |    FROM
       |      ${showSrcDests( itemTransport.sources, "<-" )}
       |    TO
       |      ${showSrcDests( itemTransport.destinations, "->" )}""".stripMargin

  private def showItemFlow( kv: ( ClassName[Item], NonEmptyVector[ItemTransport] ) ): String =
    s"""  ${kv._1}
       |${kv._2.map( showItemTransport ).mkString_( "\n" )}""".stripMargin

  private def showRefEnd( endProcesses: Option[NonEmptyVector[ProcessSplitId]] ): String =
    endProcesses
      .cata( _.toVector, Vector.empty )
      .map( id => show"#$id" )
      .mkString_( "\n      " )

  private def showItemTransportRef( itemTransport: ItemTransportRef ): String =
    s"""  - FROM
       |      ${showRefEnd( itemTransport.ends.get( FlowEnd.Source ) )}
       |    TO
       |      ${showRefEnd( itemTransport.ends.get( FlowEnd.Destination ) )}""".stripMargin

  private def showTransportSplit( transportSplit: TransportSplit ): String =
    f"  + ${transportSplit.from + 1}  -(${transportSplit.amount}%.3f)-> #${transportSplit.to + 1}"

  private def showItemFlowRef( kv: ( ClassName[Item], ItemFlows ) ): String =
    s"""  ${kv._1}
       |${kv._2.transports.map( showItemTransportRef ).mkString_( "\n" )}
       |${kv._2.transportSplits.map( showTransportSplit ).mkString_( "\n" )}""".stripMargin

  private def showProdRecipes: String =
    self.prod.productionRows
      .map: cr =>
        f"${cr.fractionalAmount}%.3f ${cr.recipe.className}%s"
      .mkString_( "\n  " )

  private def showEndSplit( kv: ( ProcessSplitId, ( Double, Group, EndId ) ) ): String =
    show"  #${kv._1}: ${kv._2._3} x ${kv._2._1} [${kv._2._2}]"

  override def toString: String =
    s"""FLOWS
       |Prod Recipes
       |  $showProdRecipes
       |Splits
       |${self.endSplits.to( SortedMap ).map( showSplits ).mkString( "\n" )}
       |By Split id
       |${self.endsBySplitId.to( SortedMap ).map( showEndSplit ).mkString( "\n" )}
       |Transports (ref)
       |${self.itemFlows.to( SortedMap ).map( showItemFlowRef ).mkString( "\n" )}
       |Transports
       |${self.itemTransports.to( SortedMap ).map( showItemFlow ).mkString( "\n" )}
       |""".stripMargin
