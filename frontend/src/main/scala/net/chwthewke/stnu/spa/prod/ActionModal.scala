package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyList
import cats.syntax.all.*

import data.Countable
import model.Item
import model.Transport
import model.prod.FlowEnd

sealed trait ActionModal

object ActionModal:
  case class SplitAction(
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]],
      local: ItemTransport,
      oppositePeers: List[Countable[Double, Split[SrcDest]]],
      equalSplitCount: Option[Int],
      machineCount: Option[( Int, Int )] // curr, max
  ) extends ActionModal:
    def transport: Transport = local.transport

    def equal: SplitType                       = SplitType.Equal( equalSplitCount )
    def equalFixed: SplitType                  = SplitType.EqualFixed( equalSplitCount )
    def byMachineCount: SplitType.MachineCount = SplitType.MachineCount( machineCount._1F )
    def even: SplitType                        = SplitType.Even
    def remainder: SplitType                   = SplitType.Remainder
    def max: SplitType                         = SplitType.Max
    def maxAll: SplitType                      = SplitType.MaxAll
    def opposite: List[SplitType.Opposite]     =
      if ( oppositePeers.length > 1 )
        oppositePeers.map( SplitType.Opposite( _ ) )
      else Nil

  // TODO could move some logic from Flows to here (from both *actionModal() & previewSplit/previewMerge)
  // also there might be some duplication in FlowViews.*srcDestModal
  object SplitAction:
    def apply(
        pos: SrcDestPos,
        srcDest: Countable[Double, Split[SrcDest]],
        local: ItemTransport,
        oppositePeers: List[Countable[Double, Split[SrcDest]]]
    ): SplitAction =
      val amount: Double    = srcDest.amount
      val unit: Int         = local.transport.perMinute
      val machineCount: Int = srcDest.item.value.process.foldMap( _.machineCount )
      SplitAction(
        pos,
        srcDest,
        local,
        oppositePeers,
        Option.when( amount > unit )( ( amount / unit.toDouble ).ceil.toInt ),
        Option.when( machineCount > 1 )( ( 1, machineCount / 2 ) )
      )

  case class MergeAction(
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]],
      transport: Transport,
      adjacentPeers: List[List[Countable[Double, Split[SrcDest]]]]
  ) extends ActionModal:
    def local: MergeType                   = MergeType.Local
    def global: MergeType                  = MergeType.Global
    def adjacent: List[MergeType.Adjacent] =
      adjacentPeers.flatten
        .filter: peer =>
          peer.item.original == srcDest.item.original &&
            peer.item.split != srcDest.item.split
        .map( MergeType.Adjacent( _ ) )

  case class SplitTransportAction(
      item: ClassName[Item],
      index: Int,
      flowEnd: FlowEnd,
      amount: Double,
      targets: NonEmptyList[Int]
  ) extends ActionModal
