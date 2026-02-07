package net.chwthewke.stnu
package spa
package prod

import alleycats.std.iterable.*
import cats.Monoid
import cats.data.NonEmptyVector
import cats.derived.*
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.Countable
import model.Item
import model.prod.FlowEnd
import model.prod.Group

/**
 * The flows fo a group are:
 *   - by flow end (direction)
 *   - by item
 *   - all the flows to or from it
 *   - AND all the flows to and from one of its descendants (where the other end it not another descendant)
 * Each flow, matching an ItemTransport, has:
 *   - 1 or more remote ends (groups, parent or sibling)
 *   - 1 or more local end (a child) group or 1+ process(es)
 *     - and possibly other groups on the same end of the transport
 *   - or both ends might be local (only if other groups are involved) -
 * NOTE
 *   - flows.get(FlowEnd.Source) are exports
 *   - flows.get(FlowEnd.Destination) are imports
 */
case class GroupFlows(
    flows: Map[FlowEnd, Map[Item, NonEmptyVector[GroupTransport]]],
    flatFlows: Map[FlowEnd, Map[Item, GroupTransport]]
)

object GroupFlows:
  def apply( flows: Flows, group: Group ): GroupFlows =
    val groupItemFlows: Map[FlowEnd, Map[Item, NonEmptyVector[GroupItemFlows]]] =
      ( flows.itemFlows: Iterable[( ClassName[Item], NonEmptyVector[ItemTransport] )] )
        .foldMap:
          case ( itemClass, transports ) =>
            flows.prod.env
              .getItem( itemClass )
              .foldMap( item => transports.foldMap( itemTransportGroupFlows( flows, group, item, _ ) ) )

    GroupFlows(
      groupItemFlows.fmap:
        _.flatMap:
          case ( item, transports ) =>
            transports.toVector.mapFilter( _.transport( flows, item ) ).toNev.tupleLeft( item )
      ,
      groupItemFlows.fmap:
        _.flatMap:
          case ( item, transports ) => transports.combineAll.transport( flows, item ).tupleLeft( item )
    )

  // for 1 ItemTransport, then possibly combined
  private case class GroupItemFlows(
      local: SortedMap[LocalGroupEnd, Double],
      localGroups: SortedSet[GroupEnd],
      remote: SortedMap[RemoteGroupEnd, Double]
  ) derives Monoid:
    private def localAdjacentGroups: Int =
      localGroups.count( !local.keySet.map( GroupEnd.Local( _ ) ).contains( _ ) )

    def transport( flows: Flows, item: Item ): Option[GroupTransport] =
      (
        local.toVector.map { case ( localEnd, amount ) => Countable( localEnd, amount ) }.toNev,
        remote.toVector.map { case ( remoteEnd, amount ) => Countable( remoteEnd, amount ) }.toNev
      ).mapN: ( localEnds, remoteEnds ) =>
        GroupTransport(
          flows.prod.selectTransport( item, localEnds.foldMap( _.amount ) ),
          localEnds,
          localAdjacentGroups,
          remoteEnds
        )

  def groupEndOf( flows: Flows, group: Group, split: Split[SrcDest] ): GroupEnd =
    split.end match
      case EndId.Input( item )     => GroupEnd.Remote( RemoteGroupEnd.Input )
      case EndId.Requested( item ) => GroupEnd.Remote( RemoteGroupEnd.Requested )
      case EndId.Byproduct( item ) => GroupEnd.Remote( RemoteGroupEnd.Byproduct )
      case EndId.Process( _ )      =>
        val toSplitGroup: Group = group.nearestGroupTo( split.group )

        if ( toSplitGroup == group )
          GroupEnd.Local( LocalGroupEnd.Process( split.id ) )
        else if ( toSplitGroup.isDescendantOf( group ) )
          GroupEnd.Local( LocalGroupEnd.Child( toSplitGroup ) )
        else
          GroupEnd.Remote( RemoteGroupEnd.OtherGroup( toSplitGroup ) )

  private def itemTransportGroupFlows(
      flows: Flows,
      group: Group,
      item: Item,
      transport: ItemTransport
  ): Map[FlowEnd, Map[Item, NonEmptyVector[GroupItemFlows]]] =

    // CONDITIONS TO INCLUDE FLOW (ItemTransport)
    // source       is (part) local -> export from local sources to non-local & lower-level local destinations
    // destination  "     "     "   -> import to local destinations from non-local & lower-level local sources
    // but no flows between children

    def importExportFlows( direction: FlowEnd ): GroupItemFlows =
      val directionFlowsByGroup: SortedMap[GroupEnd, Double] =
        transport
          .get( direction )
          .foldMap: cs =>
            val groupEnd: GroupEnd = groupEndOf( flows, group, cs.item )
            SortedMap( groupEnd -> cs.amount )

      val eligibleLocalEndFlows: SortedMap[LocalGroupEnd, Double] =
        directionFlowsByGroup.collect:
          case ( GroupEnd.Local( localEnd ), split ) => ( localEnd, split )

      val groupIsEligilble: Boolean = eligibleLocalEndFlows.keys.exists:
        case _: LocalGroupEnd.Child   => false
        case _: LocalGroupEnd.Process => true

      val eligibleRemoteEndFlows: SortedMap[RemoteGroupEnd, Double] =
        transport
          .get( direction.opposite )
          .foldMap: cs =>
            val groupEnd: GroupEnd                  = groupEndOf( flows, group, cs.item )
            val destination: Option[RemoteGroupEnd] =
              groupEnd match
                case GroupEnd.Remote( end )                              => end.some
                case GroupEnd.Local( LocalGroupEnd.Child( childGroup ) ) =>
                  Option.when( groupIsEligilble )( RemoteGroupEnd.OtherGroup( childGroup ) )
                case GroupEnd.Local( _ ) => none
            destination.foldMap: remote =>
              SortedMap( remote -> cs.amount )

      GroupItemFlows( eligibleLocalEndFlows, directionFlowsByGroup.keySet, eligibleRemoteEndFlows )

    FlowEnd.cases.fproduct( flowEnd => Map( item -> NonEmptyVector.one( importExportFlows( flowEnd ) ) ) ).toMap
