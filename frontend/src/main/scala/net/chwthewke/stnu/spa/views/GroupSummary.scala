package net.chwthewke.stnu
package spa
package views

import cats.Semigroup
import cats.data.NonEmptyVector
import cats.syntax.all.*
import tyrian.Html

import data.Countable
import model.Footprint
import model.Item
import model.Machine
import model.prod.FlowEnd
import model.prod.Group
import spa.css.Bulma
import spa.css.Phosphor
import spa.prod.Flows
import spa.prod.GroupFlows
import spa.prod.GroupTransport
import spa.prod.Groups
import spa.prod.LocalGroupEnd
import spa.prod.RemoteGroupEnd
import spa.prod.footprints

object GroupSummary:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( flows: Flows, groups: Groups, group: Group, groupFlows: GroupFlows, flat: Boolean ): Html[Nothing] =
    Html.div(
      importExports( flows, groups, group, groupFlows, flat ),
      machinesSummary( flows, groups, group )
    )

  private def getEndFlows[A]( byEnd: Map[FlowEnd, Map[Item, A]], flowEnd: FlowEnd )(
      f: A => NonEmptyVector[GroupTransport]
  ): Map[Item, NonEmptyVector[GroupTransport]] =
    byEnd.get( flowEnd ).foldMap( _.fmap( f ) )

  private def getEndFlows(
      groupFlows: GroupFlows,
      flowEnd: FlowEnd,
      flat: Boolean
  ): Map[Item, NonEmptyVector[GroupTransport]] =
    if ( flat )
      getEndFlows( groupFlows.flatFlows, flowEnd )( NonEmptyVector.one )
    else
      getEndFlows( groupFlows.flows, flowEnd )( identity )

  private def importExports(
      flows: Flows,
      groups: Groups,
      group: Group,
      groupFlows: GroupFlows,
      flat: Boolean
  ): Html[Nothing] =
    Html.div( b.block )(
      Html.h2( b.title + b.isSize4 + b.hasTextCentered )(
        s"Import/exports of ${FlowElements.longGroupName( group )}"
      ),
      Html.div( b.columns )(
        Html.div( b.column + b.isHalf )(
          Html.h3( b.subtitle + b.isSize4 + b.hasTextCentered )( "IMPORTS" ),
          endGroupFlows( flows, groups, groupFlows, FlowEnd.Destination, flat )
        ),
        Html.div( b.column + b.isHalf )(
          Html.h3( b.subtitle + b.isSize4 + b.hasTextCentered )( "EXPORTS" ),
          endGroupFlows( flows, groups, groupFlows, FlowEnd.Source, flat )
        )
      )
    )

  private def endGroupFlows(
      flows: Flows,
      groups: Groups,
      groupFlows: GroupFlows,
      flowEnd: FlowEnd,
      flat: Boolean
  ): Html[Nothing] =
    Html.div(
      getEndFlows( groupFlows, flowEnd, flat ).toList
        .map:
          case ( item, transports ) =>
            groupTransports( flows, groups, flowEnd, item, transports, flat )
    )

  private def groupTransports(
      flows: Flows,
      groups: Groups,
      flowEnd: FlowEnd,
      item: Item,
      transports: NonEmptyVector[GroupTransport],
      flat: Boolean
  ): Html[Nothing] =
    val groupTransportsWithIndex: List[( GroupTransport, Option[Int] )] =
      (
        if ( flat )
          transports.tupleRight( none )
        else
          transports.zipWithIndex.map { case ( t, ix ) => ( t, ix.some ) }
      ).toVector.toList
    Html.div( b.block )(
      Html.h4( b.subtitle + b.hasTextCentered + b.hasBackgroundGreyDarker + b.hasTextLight )(
        icon.verticalAlign().withClasses( b.pr2 ).withSize( b.is32x32 ).item( flows.prod.env, item ),
        Html.text( item.displayName )
      )
        ::
          groupTransportsWithIndex.map {
            case ( transport, ix ) => groupTransport( flows, groups, flowEnd, transport, ix, flat )
          }
    )

  private def groupTransport(
      flows: Flows,
      groups: Groups,
      flowEnd: FlowEnd,
      groupTransport: GroupTransport,
      index: Option[Int],
      flat: Boolean
  ): Html[Nothing] = {

    val local: Html[Nothing] = Html.div( b.column + b.is8 )(
      Elements.messageCenteredHeader( b.isSuccess, b.isSize7 + b.hasTextSuccessDark, Html.text( "Local" ) )(
        Html.div( b.messageBody )( localTable( flows, groups, groupTransport ) )
      )
    )

    val remote: Html[Nothing] = Html.div( b.column + b.is4 )(
      Elements.messageCenteredHeader( b.isInfo, b.isSize7 + b.hasTextInfoDark, Html.text( "Remote" ) )(
        Html.div( b.messageBody )( remoteTable( flows, groups, groupTransport ) )
      )
    )

    val columns: Html[Nothing] = flowEnd match
      case FlowEnd.Source      => Html.div( b.columns )( local, remote )
      case FlowEnd.Destination => Html.div( b.columns )( remote, local )

    Html.div( b.block )(
      Html.div( b.box )(
        FlowElements.transportHeader( flows.prod.env, groupTransport, index, warnings = !flat ),
        columns
      )
    )
  }

  def localTable( flows: Flows, groups: Groups, groupTransport: GroupTransport ): Html[Nothing] =
    Html.table( b.table + b.isFullwidth )(
      groupTransport.localEnds.toVector.toList.map( localRow( flows, groups, _ ) ) ++
        Option.when( groupTransport.localAdjacent > 0 )(
          Html.tr(
            Html.td( b.hasTextCentered + b.hasTextWeightBold + b.isItalic )(
              s"+${groupTransport.localAdjacent}"
            ),
            Html.td( b.hasTextRight )( "*" )
          )
        )
    )

  def localRow( flows: Flows, groups: Groups, localEnd: Countable[Double, LocalGroupEnd] ): Html[Nothing] =
    Html.tr(
      Html.td(
        localEnd.item match
          case LocalGroupEnd.Process( splitId ) => Html.text( flows.getSplit( splitId ).displayName )
          case LocalGroupEnd.Child( group )     => FlowElements.groupButton( groups, group, newGroup = false, none )
      ),
      Html.td( b.hasTextRight )( Numbers.showDouble3( localEnd.amount ) )
    )

  def remoteTable( flows: Flows, groups: Groups, groupTransport: GroupTransport ): Html[Nothing] =
    Html.table( b.table + b.isFullwidth )(
      groupTransport.remoteEnds.toVector.toList.map( remoteRow( flows, groups, _ ) )
    )

  def remoteRow( flows: Flows, groups: Groups, remoteEnd: Countable[Double, RemoteGroupEnd] ): Html[Nothing] =
    Html.tr(
      Html.td( Numbers.showDouble3( remoteEnd.amount ) ),
      Html.td( b.hasTextRight )(
        remoteEnd.item match
          case RemoteGroupEnd.OtherGroup( group ) => FlowElements.groupButton( groups, group, newGroup = false, none )
          case RemoteGroupEnd.Input               => Html.strong( "INPUT" )
          case RemoteGroupEnd.Requested           => Html.strong( "REQUESTED" )
          case RemoteGroupEnd.Byproduct           => Html.strong( "BYPRODUCT" )
      )
    )

  private def getMachineCounts( flows: Flows, group: Group ): List[( Machine, Int )] =
    flows.endSplits
      .unorderedFoldMap:
        _.splits.toVector
          .collect:
            case ( splitId, ( fraction, splitGroup ) ) if splitGroup == group =>
              ( flows.getSplit( splitId ), fraction )
          .foldMap:
            case ( split, fraction ) =>
              split.original.process.foldMap( cr => Map( cr.recipe.producedIn -> cr.times( fraction ).machineCount ) )
      .toList
      .sortBy:
        case ( machine, _ ) => ( machine.tier, machine.displayName )

  private def blockFootprint[B: Semigroup]( wrap: Footprint => B )( machineCounts: List[( Machine, Int )] ): Option[B] =
    machineCounts
      .foldMap:
        case ( machine, count ) =>
          machine.footprint
            .flatMap: footprint =>
              Option.when( count > 0 )( wrap( footprint ).combineN( count ) )

  // NOTE
  //   - for leaf groups, computes the footprint as if it were a single row of machines
  //   - for non-leaf groups which have only leaf children, computes the footprint by putting the children's
  //      rows (plus one row for this group's machines if any) next to each other
  private def groupFootprint(
      flows: Flows,
      groups: Groups,
      group: Group,
      machineCounts: List[( Machine, Int )]
  ): Option[Footprint] =
    groups
      .get( group.path )
      .flatMap:
        case Groups.Nil =>
          // leaf group -> row footprint
          blockFootprint( footprints.Row( _ ) )( machineCounts ).map( _.footprint )
        case subgroups @ Groups.SubGroups( children ) if subgroups.depth == 2 =>
          // all children are leaf groups -> floor footprint
          val byRowMachineCounts: List[List[( Machine, Int )]] =
            machineCounts ::
              children.keySet.foldMap( n => List( getMachineCounts( flows, Group( group.path :+ n ) ) ) )
          byRowMachineCounts
            .map( mc => blockFootprint( footprints.Row( _ ) )( mc ).map( _.footprint ) )
            .flattenOption
            .foldMap( rowFootprint => footprints.Floor( rowFootprint ).some )
            .map( _.footprint )
        case _ => none

  private def machinesSummary( flows: Flows, groups: Groups, group: Group ): Html[Nothing] =
    val machineCounts: List[( Machine, Int )] = getMachineCounts( flows, group )

    val simpleFootprint: Option[Html[Nothing]] =
      groupFootprint( flows, groups, group, machineCounts )
        .map: footprint =>
          Elements.messageCenteredHeader( b.isPrimary, b.hasTextPrimaryDark, Html.text( "Simple footprint" ) )(
            FootprintElements.text( footprint )
          )

    Html.div( b.hasTextCentered + b.container + b.mb5 )(
      Html.h2( b.title + b.isSize4 + b.hasTextCentered )(
        s"Machines in ${FlowElements.longGroupName( group )}"
      ),
      Html.div( b.block )(
        Html.div( b.grid + b.isGap8 )(
          machineCounts.map:
            case ( machine, count ) =>
              Html.div( b.cell )(
                RecipeFrag.numberedIcon( flows.prod.env, Countable( machine, count ), None ),
                Html.span( b.ml2 )( machine.displayName )
              )
        )
      ),
      simpleFootprint
    )
