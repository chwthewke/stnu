package net.chwthewke.stnu
package spa
package prod

import data.Countable
import model.Transport

sealed trait ActionModal

object ActionModal:
  case class SplitAction(
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]],
      local: ItemTransport,
      oppositePeers: List[Countable[Double, Split[SrcDest]]],
      equalSplitCount: Option[Int]
  ) extends ActionModal:
    def transport: Transport = local.transport.item

    def equal: SplitType                   = SplitType.Equal( equalSplitCount )
    def even: SplitType                    = SplitType.Even
    def remainder: SplitType               = SplitType.Remainder
    def max: SplitType                     = SplitType.Max
    def maxAll: SplitType                  = SplitType.MaxAll
    def opposite: List[SplitType.Opposite] = oppositePeers.map( SplitType.Opposite( _ ) )

  // TODO could move some logic from Flows to here (from both *actionModal() & previewSplit/previewMerge)
  // also there might be some duplication in FlowViews.*srcDestModal
  object SplitAction:
    def apply(
        pos: SrcDestPos,
        srcDest: Countable[Double, Split[SrcDest]],
        local: ItemTransport,
        oppositePeers: List[Countable[Double, Split[SrcDest]]]
    ): SplitAction =
      val amount = srcDest.amount
      val unit   = local.transport.item.perMinute
      SplitAction(
        pos,
        srcDest,
        local,
        oppositePeers,
        Option.when( amount > unit )( ( amount / unit.toDouble ).ceil.toInt )
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
