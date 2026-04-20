package net.chwthewke.stnu
package spa
package prod

import cats.Order
import cats.syntax.all.*

import model.prod.Group
import protocol.persistence.ProcessSplitId

enum GroupEnd:
  case Local( end: LocalGroupEnd )
  case Remote( end: RemoteGroupEnd )

  def isLocal: Boolean = this match
    case _: GroupEnd.Local  => true
    case _: GroupEnd.Remote => false

object GroupEnd:
  given Order[GroupEnd]:
    override def compare( x: GroupEnd, y: GroupEnd ): Int =
      x match
        case GroupEnd.Local( xEnd ) =>
          y match
            case GroupEnd.Local( yEnd ) => xEnd.compare( yEnd )
            case _                      => -1
        case GroupEnd.Remote( xEnd ) =>
          y match
            case GroupEnd.Remote( yEnd ) => xEnd.compare( yEnd )
            case GroupEnd.Local( end )   => 1

  given Ordering[GroupEnd] = Order.catsKernelOrderingForOrder

// TODO Local/RemoteGroupEnd should include other transports via transport splits (not to be displayed in flat mode)
enum LocalGroupEnd:
  case Child( group: Group )
  case Process( splitId: ProcessSplitId )

object LocalGroupEnd:
  given Order[LocalGroupEnd] =
    Order.by:
      case LocalGroupEnd.Child( group )     => ( 1, group.path, 0 )
      case LocalGroupEnd.Process( splitId ) => ( 0, Vector.empty, splitId.id )
  given Ordering[LocalGroupEnd] = Order.catsKernelOrderingForOrder

enum RemoteGroupEnd:
  case OtherGroup( group: Group )
  case Input
  case Requested
  case Byproduct

object RemoteGroupEnd:
  given Order[RemoteGroupEnd] =
    Order.by:
      case RemoteGroupEnd.OtherGroup( group ) => ( 0, group.path )
      case RemoteGroupEnd.Input               => ( 1, Vector.empty )
      case RemoteGroupEnd.Requested           => ( 2, Vector.empty )
      case RemoteGroupEnd.Byproduct           => ( 3, Vector.empty )
  given Ordering[RemoteGroupEnd] = Order.catsKernelOrderingForOrder
