package net.chwthewke.stnu
package spa
package prod

import data.Countable

enum FlowAction:
  case Reset
  case MoveSrcDest( pos: SrcDestPos, amount: Int, bump: Boolean ) // TODO amount is +/- 1, maybe constrain the type further
  case StartSplitSrcDest( pos: SrcDestPos )
  case SplitSrcDest( pos: SrcDestPos, splitType: SplitType )
  case SplitEqualSetCount( count: Int )
  case StartMergeSrcDest( pos: SrcDestPos )
  case MergeSrcDest( pos: SrcDestPos, mergeType: MergeType )
  case AbortScrDestOp

enum SplitType:
  case Even
  case Equal( default: Option[Int] )
  case Remainder
  case Max
  case MaxAll
  case Opposite( split: Countable[Double, Split[SrcDest]] )

  override def toString: String =
    this match
      case Even              => "Even"
      case Equal( _ )        => "Equal"
      case Remainder         => "Remainder"
      case Max               => "Max"
      case MaxAll            => "MaxAll"
      case Opposite( split ) => f"Opposite ${split.item.displayName} (${split.amount}%.3f)"

enum MergeType:
  case Local
  case Global
  case Adjacent( split: Countable[Double, Split[SrcDest]] )

  override def toString: String =
    this match
      case Local             => "Local"
      case Global            => "Global"
      case Adjacent( split ) => f"Adjacent ${split.item.displayName} (${split.amount}%.3f)"
