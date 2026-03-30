package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyVector
import cats.syntax.all.*
import tyrian.Html

import data.Countable
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
import spa.prod.IntegratedFootprint
import spa.prod.LocalGroupEnd
import spa.prod.RemoteGroupEnd

object GroupSummary:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( flows: Flows, groups: Groups, group: Group, groupFlows: GroupFlows, flat: Boolean ): Html[Nothing] =
    Html.div(
      machinesSummary( flows, groups, group ),
      importExports( flows, groups, group, groupFlows, flat )
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
  ): Option[Html[Nothing]] =
    val imports: Option[Html[Nothing]] = endGroupFlows( flows, groups, groupFlows, FlowEnd.Destination, flat )
    val exports: Option[Html[Nothing]] = endGroupFlows( flows, groups, groupFlows, FlowEnd.Source, flat )

    Option.when( imports.isDefined || exports.isDefined )(
      Html.div( b.block )(
        Html.h2( b.title + b.isSize4 + b.hasTextCentered )(
          s"Import/exports of ${FlowElements.longGroupName( group )}"
        ),
        Html.div( b.columns )(
          Html.div( b.column + b.isHalf )(
            Html.h3( b.subtitle + b.isSize4 + b.hasTextCentered )( "IMPORTS" ),
            imports
          ),
          Html.div( b.column + b.isHalf )(
            Html.h3( b.subtitle + b.isSize4 + b.hasTextCentered )( "EXPORTS" ),
            exports
          )
        )
      )
    )

  private def endGroupFlows(
      flows: Flows,
      groups: Groups,
      groupFlows: GroupFlows,
      flowEnd: FlowEnd,
      flat: Boolean
  ): Option[Html[Nothing]] =
    val endFlows: List[( Item, NonEmptyVector[GroupTransport] )] = getEndFlows( groupFlows, flowEnd, flat ).toList
      .sortBy( _._1.displayName )
    Option.when( endFlows.nonEmpty )(
      Html.div(
        endFlows
          .map:
            case ( item, transports ) =>
              groupTransports( flows, groups, flowEnd, item, groupFlows.balance.get( item ).flatten, transports, flat )
      )
    )

  private def groupTransports(
      flows: Flows,
      groups: Groups,
      flowEnd: FlowEnd,
      item: Item,
      balance: Option[Double],
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
      Html.h4( b.subtitle + b.hasTextCentered + b.hasBackgroundGreyDarker + b.hasTextLight + b.py1 )(
        icon.verticalAlign().withClasses( b.mr2 ).withSize( b.is32x32 ).item( flows.prod.env, item ),
        Html.span( va() )( item.displayName ),
        Html.div( b.help )(
          balance match
            case Some( value ) =>
              val flowDirection = if ( value > 0 ) "exporter" else "importer"
              s"Net $flowDirection: ${Numbers.showDouble3( value.abs )}"
            case None => "Net neutral"
        )
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

  private def groupMachines(
      flows: Flows,
      groups: Groups,
      group: Group
  ): ( List[Countable[Int, Machine]], Option[IntegratedFootprint] ) =
    def localMachinesAndFootprint: ( List[Countable[Int, Machine]], Option[IntegratedFootprint] ) =
      val machineCounts: List[( Machine, Int )] = getMachineCounts( flows, group )
      (
        machineCounts.map { case ( machine, count ) => Countable( machine, count ) },
        machineCounts.foldMap:
          case ( machine, count ) =>
            Option.when( count > 0 )( machine.footprint.map( IntegratedFootprint.row ).combineN( count ) ).flatten
      )

    groups
      .get( group.path )
      .foldMap:
        case Groups.Nil                   => localMachinesAndFootprint
        case Groups.SubGroups( children ) =>
          val ( childrenMachines, childrenFootprints ) =
            children.keys.toVector
              .map: i =>
                val ( childMachines, childFootprint ) =
                  groupMachines( flows, groups, Group( group.path :+ i ) )
                ( childMachines, childFootprint.integrate )
              .combineAll

          val ( localMachines, localFootprint ) = localMachinesAndFootprint
          ( localMachines |+| childrenMachines, localFootprint |+| childrenFootprints )

  private def machinesSummary( flows: Flows, groups: Groups, group: Group ): Option[Html[Nothing]] =
    val ( machines, footprint ) = groupMachines( flows, groups, group )

    val machineCounts: List[( Machine, Int )] =
      machines.gather
        .sortBy( cm => ( cm.item.machineType, cm.item.tier, cm.item.displayName ) )
        .map( cm => ( cm.item, cm.amount ) )

    val simpleFootprint: Option[Html[Nothing]] =
      footprint
        .map: integratedFootprint =>
          val integrationLevel: String = integratedFootprint.level.displayName
          Elements.messageCenteredHeader(
            b.isPrimary,
            b.hasTextPrimaryDark,
            Html.text( s"Footprint estimate ($integrationLevel)" )
          )(
            FootprintElements.text( integratedFootprint.footprint )
          )

    Option.when( machineCounts.nonEmpty || simpleFootprint.isDefined )(
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
    )
