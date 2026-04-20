package net.chwthewke.stnu
package spa
package prod

import data.Countable
import model.Item
import model.prod.FlowEnd

enum FlowAction:
  case Reset
  case MoveSrcDest( pos: SrcDestPos, amount: Int, bump: Boolean ) // TODO amount is +/- 1, maybe constrain the type further
  case StartSplitSrcDest( pos: SrcDestPos )
  case SplitSrcDest( pos: SrcDestPos, splitType: SplitType )
  case SplitEqualSetCount( count: Int )
  case SplitByMachineSetCount( count: Int )
  case StartMergeSrcDest( pos: SrcDestPos )
  case MergeSrcDest( pos: SrcDestPos, mergeType: MergeType )
  case StartSplitTransport( modal: ActionModal.SplitTransportAction )
  case SplitTransport( item: ClassName[Item], index: Int, flowEnd: FlowEnd, target: Int, amount: Double )
  case DeleteTransportSplit( item: ClassName[Item], transportIndex: Int, flowEnd: FlowEnd, peerIndex: Int )
  case AbortModalFlowOp

enum SplitType:
  case Even
  case Equal( default: Option[Int] )
  case EqualFixed( default: Option[Int] )
  case MachineCount( counts: Option[Int] )
  case Remainder
  case Max
  case MaxAll
  case Opposite( split: Countable[Double, ItemTransport.Peer[SrcDest]] )

enum MergeType:
  case Local
  case Global
  case Adjacent( split: Countable[Double, Split[SrcDest]] )
