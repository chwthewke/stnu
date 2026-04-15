package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyVector
import cats.syntax.all.*
import tyrian.Attr
import tyrian.CSS
import tyrian.Elem
import tyrian.Html

import data.Countable
import model.Item
import model.Transport
import model.prod.FlowEnd
import spa.css.Bulma
import spa.css.Classes
import spa.css.CssClass
import spa.css.Phosphor
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.prod.ActionModal
import spa.prod.ClockedRecipe
import spa.prod.FlowAction
import spa.prod.Flows
import spa.prod.Groups
import spa.prod.ItemTransport
import spa.prod.MergeType
import spa.prod.ProdModel
import spa.prod.Split
import spa.prod.SplitMergePreview
import spa.prod.SplitType
import spa.prod.SrcDest
import spa.prod.SrcDestPos

object FlowsView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def openButton( model: PlanModel ): Elem[Nothing] =
    model.flows.toOption.map: flows =>
      val ( hasOverflow, hasUnbalanced ) =
        flows.itemTransports.values.foldLeft( ( false, false ) ):
          case ( ( overflow, unbalanced ), transports ) =>
            ( overflow || transports.exists( _.overflow ), unbalanced || transports.exists( !_.balanced ) )

      Html.a(
        b.button + b.isLink,
        Html.href := model.getLocation.copy( organizer = true ).toInternalLocation,
        Html.disabled( model.flows.isLeft )
      )(
        Html.span( b.iconText )(
          Option.when( hasOverflow )( Html.span( b.icon )( Html.i( p.fill.warning + b.hasTextWarning )() ) ),
          Option.when( hasUnbalanced )( Html.span( b.icon )( Html.i( p.fill.notEquals + b.hasTextDanger )() ) ),
          Html.span( "Organize" ),
          Html.span( b.icon )( Html.i( p.regular.funnel )() )
        )
      )

  def closeButton( model: PlanModel ): Html[Nothing] =
    Html.a( b.button + b.isLink, Html.href := model.getLocation.copy( organizer = false ).toInternalLocation )(
      Html.span( b.iconText )( Html.span( "Back" ), Html.span( b.icon )( Html.i( p.regular.arrowUDownLeft )() ) )
    )

  private val reset: Html[PlanMsg] =
    Html
      .button(
        b.button + b.isDanger,
        Html.onClick( FlowAction.Reset )
      )(
        Html.span( b.iconText )(
          Html.span( "Reset" ),
          Html.span( b.icon )( Html.i( p.regular.trash )() )
        )
      )
      .map( PlanMsg.Flow( _ ) )

  private def toggleShow( prodUi: ProdModel.Ui ): Html[PlanMsg] =
    Html
      .button(
        b.button,
        Html.onClick( PlanMsg.ToggleShowAllFlows( !prodUi.showAllFlows ) )
      )(
        Html.span( b.iconText )(
          Html.span( if ( prodUi.showAllFlows ) "All" else "Overflow" ),
          Html.span( b.icon )(
            Html.i( if ( prodUi.showAllFlows ) p.regular.eye else p.regular.eyeSlash )()
          )
        )
      )

  def apply( model: PlanModel, panelButtons: Html[PlanMsg]* ): List[Html[PlanMsg]] =
    val flows: Option[Flows] = model.flows.toOption

    val itemFlows: List[( Item, NonEmptyVector[ItemTransport] )] =
      flows.foldMap:
        _.itemTransports.toList
          .mapFilter:
            case ( itemClass, itemTransports ) =>
              model.env.getItem( itemClass ).tupleRight( itemTransports )
          .sortBy( _._1.displayName )
    val groups: Groups = flows.foldMap( Groups.of )

    def planHeader: List[Html[PlanMsg]] =
      PlanHeader( model, panelButtons :+ reset :+ toggleShow( model.productionUi )* )

    def itemTags: Html[PlanMsg] = Html.div( b.block )( itemLinks( model.env, model.productionUi, itemFlows ) )

    def itemFlowBlocks: List[Html[PlanMsg]] =
      flows
        .map: f =>
          Html.div(
            itemFlows
              .filter:
                case ( _, trs ) =>
                  model.productionUi.showAllFlows || trs.exists( it => it.overflow || !it.balanced )
              .map:
                case ( item, transports ) =>
                  displayItem( f, groups, item, transports )
          )
        .toList

    List(
      Html.div( b.mb6 )(
        flows.flatMap( modal ) ++:
          planHeader ++:
          itemTags +:
          itemFlowBlocks ++: Nil
      )
    )

  private def itemAnchorId( item: Item ): String =
    show"fa_${item.className}"

  private def itemAnchor( item: Item ): Attr[Nothing] =
    Html.id := itemAnchorId( item )

  private def itemLinks(
      env: Env,
      ui: ProdModel.Ui,
      itemFlows: List[( Item, NonEmptyVector[ItemTransport] )]
  ): List[Html[PlanMsg]] =
    itemFlows.foldMap:
      case ( item, itemTransports ) =>
        val unbalanced = itemTransports.exists( !_.balanced )
        val overflow   = itemTransports.exists( _.overflow )
        Option
          .when( ui.showAllFlows || unbalanced || overflow ):
            val color: CssClass =
              if ( unbalanced ) b.isDanger
              else if ( overflow ) b.isWarning
              else b.isDark

            Html.span(
              b.tag + b.m1 + color,
              Html.style( CSS.cursor( "pointer" ) ),
              Html.onClick( PlanMsg.MoveTo( Some( itemAnchorId( item ) ) ) )
            )(
              Html.span( b.iconText )( item.displayName ),
              Html.span( b.icon )( icon.verticalAlign().withDropShadow().item( env, item ) )
            )
          .toList

  private def renderSrcDestMachines( env: Env, srcDest: Split[SrcDest] ): List[Elem[Nothing]] =
    def renderMachines( recipe: ClockedRecipe ) =
      List(
        Html.strong( recipe.machineCount.toString ),
        icon.verticalAlign().withClasses( b.ml2 ).machine( env, recipe.recipe.producedIn )
      )
    srcDest.value match
      case SrcDest.Step( recipe )    => renderMachines( recipe )
      case SrcDest.Extract( recipe ) => renderMachines( recipe )
      case _                         => Nil

  /**
   * flow actions:
   *
   *   - move up or down within transports
   *     - can move up unless at 0
   *     - can move down unless at max index & only flow in transport
   *   - split flow
   *     - equally (if overflow, smallest n s.t 1/n not overflow)
   *     - evenly (same headroom/overflow across transports, if multiple transports)
   *     - max (one) (if overflow)
   *     - max (all) (if overflow > 2)
   *     - for opposite peer
   *     - for opposite transport
   *   - merge flows w/ same src/dest
   *     - in current transport
   *     - across transports
   */
  private def flowActionButtons(
      flows: Flows,
      item: Item,
      direction: FlowEnd,
      transport: Transport,
      index: Int,
      subIndex: Int
  ): List[Html[FlowAction]] = {
    val pos = SrcDestPos( item, direction, index, subIndex )
    List(
      Elements.miniButtonWithMod(
        b.isLink + b.isOutlined,
        s"Move to ${transport.displayName} #$index",
        p.regular.arrowFatUp
      )( m => FlowAction.MoveSrcDest( pos, -1, m.shift ) ),
      Elements.miniButtonWithMod(
        b.isLink + b.isOutlined,
        s"Move to ${transport.displayName} #${index + 2}",
        p.regular.arrowFatDown
      )( m => FlowAction.MoveSrcDest( pos, 1, m.shift ) ),
      Elements.miniButton(
        b.isPrimary + b.isOutlined,
        s"Split...",
        p.regular.splitVertical,
        Html.disabled( !flows.canSplit( pos ) )
      )(
        FlowAction.StartSplitSrcDest( pos ).some
      ),
      Elements.miniButton(
        b.isSuccess + b.isOutlined,
        s"Merge...",
        p.regular.arrowsMerge,
        Html.disabled( !flows.canMerge( pos ) )
      )(
        FlowAction.StartMergeSrcDest( pos ).some
      )
    )
  }

  private def overflowWarningCell( hasOverflow: Boolean ): Html[Nothing] =
    Html.td(
      if ( hasOverflow )
        Html.i( va(), p.fill.warning + b.hasTextWarning )()
      else
        Html.i( Html.style( CSS.paddingRight( "16px" ) ) )()
    )

  private def flowRow(
      flows: Flows,
      groups: Groups,
      item: Item,
      direction: FlowEnd,
      transport: Transport,
      index: Int,
      subIndex: Int,
      srcDest: Countable[Double, Split[SrcDest]]
  ): Html[PlanMsg] =
    val env = flows.prod.env
    Html
      .tr(
        overflowWarningCell( srcDest.amount > transport.perMinute * ( 1d + Countable.Tolerance ) ),
        Html.td(
          Html
            .span( b.buttons + b.hasAddons )( flowActionButtons( flows, item, direction, transport, index, subIndex ) )
        ),
        Html.td( b.hasTextRight )( Html.strong( Numbers.showDouble3( srcDest.amount ) ) ),
        Html.td( b.hasTextCentered )(
          if ( srcDest.item.max == 1 ) "*" else s"#${srcDest.item.split}/${srcDest.item.max}"
        ),
        Html.td( RecipeFrag.srcDestName( env )( srcDest.item.value ) ),
        Html.td( b.hasTextCentered )( renderSrcDestMachines( env, srcDest.item ) ),
        Html.td( b.hasTextRight )( FlowElements.groupButton( groups, srcDest.item.group, newGroup = false, none ) )
      )
      .map( PlanMsg.Flow( _ ) )

  private def transportSplitRow(
      item: ClassName[Item],
      amount: Double,
      transport: Transport,
      index: Int,
      direction: FlowEnd,
      peer: Transport,
      peerIndex: Int,
      splitIndex: Int
  ): Html[PlanMsg] =
    val directionText: String = direction match
      case FlowEnd.Source      => "from"
      case FlowEnd.Destination => "to"

    Html
      .tr(
        overflowWarningCell( amount > transport.perMinute * ( 1d + Countable.Tolerance ) ),
        Html.td(
          Elements.miniButton( b.isDanger + b.isOutlined, "Delete split", p.regular.`trash` )(
            FlowAction.DeleteTransportSplit( item, index, direction, splitIndex ).some
          )
        ),
        Html.td( b.hasTextRight )( Html.strong( Numbers.showDouble3( amount ) ) ),
        Html.td(),
        Html.td( Html.em( s"$directionText ${peer.displayName} #${peerIndex + 1}" ) ),
        Html.td(),
        Html.td()
      )
      .map( PlanMsg.Flow( _ ) )

  private def flowTable(
      flows: Flows,
      groups: Groups,
      item: Item,
      direction: FlowEnd,
      itemTransport: ItemTransport,
      index: Int
  ): Html[PlanMsg] = {
    val machineRows: List[Html[PlanMsg]] =
      itemTransport
        .getMachineFlows( direction )
        .toList
        .zipWithIndex
        .map:
          case ( srcDest, ix ) =>
            flowRow( flows, groups, item, direction, itemTransport.transport, index, ix, srcDest )

    val transportSplitRows: List[Html[PlanMsg]] =
      itemTransport.transportSplits
        .get( direction )
        .orEmpty
        .toList
        .zipWithIndex
        .mapFilter:
          case ( Countable( target, amount ), splitIndex ) =>
            flows.itemTransports
              .get( item.className )
              .flatMap( _.get( target ) )
              .map: peer =>
                transportSplitRow(
                  item.className,
                  amount,
                  itemTransport.transport,
                  index,
                  direction,
                  peer.transport,
                  target,
                  splitIndex
                )

    Html.table( b.table + b.isFullwidth )(
      Html.tbody(
        machineRows ++ transportSplitRows
      )
    )
  }

  private def splitTransportButton(
      flows: Flows,
      item: Item,
      index: Int,
      flowEnd: FlowEnd
  ): Html[PlanMsg] =
    val actionOrHidden: Attr[FlowAction] =
      flows
        .previewSplitTransport( item.className, index, flowEnd )
        .fold[Attr[FlowAction]]( Html.style( CSS.visibility( "hidden" ) ) ):
          case ( amount, targets ) =>
            Html.onClick(
              FlowAction
                .StartSplitTransport(
                  ActionModal.SplitTransportAction( item.className, index, flowEnd, amount, targets )
                )
            )

    Html
      .button( b.button + b.isPrimary + b.isSmall, actionOrHidden )(
        flowEnd match
          case FlowEnd.Source      => s"Split from..."
          case FlowEnd.Destination => s"Merge into..."
      )
      .map( PlanMsg.Flow( _ ) )

  private def displayItemTransport(
      flows: Flows,
      groups: Groups,
      item: Item,
      itemTransport: ItemTransport,
      index: Int
  ): Html[PlanMsg] =
    val env: Env = flows.prod.env
    Html.div( b.box )(
      Html.div( b.isFlex + b.isFlexWrapNowrap + b.isJustifyContentCenter )(
        Html.span( splitTransportButton( flows, item, index, FlowEnd.Source ) ),
        Html.span( b.isFlexGrow1 )(),
        FlowElements.transportHeader( env, itemTransport, index.some, warnings = true ),
        Html.span( b.isFlexGrow1 )(),
        Html.span( splitTransportButton( flows, item, index, FlowEnd.Destination ) )
      ),
      Html.div( b.columns )(
        Html.div( b.column )(
          Html.p( b.notification + b.isSuccess + b.isDark )(
            flowTable( flows, groups, item, FlowEnd.Source, itemTransport, index )
          )
        ),
        Html.div( b.column )(
          Html.p( b.notification + b.isInfo + b.isDark )(
            flowTable( flows, groups, item, FlowEnd.Destination, itemTransport, index )
          )
        )
      )
    )

  private def displayItem(
      flows: Flows,
      groups: Groups,
      item: Item,
      transports: NonEmptyVector[ItemTransport]
  ): Html[PlanMsg] =
    val env = flows.prod.env
    Html.div( b.block, itemAnchor( item ) )(
      Html.h4( b.subtitle )(
        icon.verticalAlign().withClasses( b.mr2 ).withSize( b.is32x32 ).item( env, item ),
        Html.text( item.displayName )
      ) ::
        Html.div( b.columns )(
          Html.div( b.column + b.isHalf + b.hasTextCentered )( Html.strong( "PRODUCED BY" ) ),
          Html.div( b.column + b.isHalf + b.hasTextCentered )( Html.strong( "CONSUMED BY" ) )
        ) ::
        transports.zipWithIndex.toVector.toList.map:
          case ( transport, ix ) => displayItemTransport( flows, groups, item, transport, ix )
    )

  private def modal( flows: Flows ): Option[Html[PlanMsg]] =
    flows.ui.actionModal.map:
      case am: ActionModal.SplitAction          => splitSrcDestModal( flows, am )
      case am: ActionModal.MergeAction          => mergeSrcDestModal( flows, am )
      case am: ActionModal.SplitTransportAction => splitTransportModal( flows, am )

  private def splitModalHeading(
      env: Env,
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]]
  ): Html[Nothing] =
    modalHeading(
      env,
      "Split",
      RecipeFrag.numberedIcon3( env, srcDest.as( pos.item ), b.mx2 ),
      pos,
      srcDest
    )

  private def mergeModalHeading(
      env: Env,
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]]
  ): Html[Nothing] =
    modalHeading(
      env,
      "Merge",
      Icons.icon.withClasses( b.mx2 ).verticalAlign().withDropShadow().item( env, pos.item ),
      pos,
      srcDest
    )

  private def modalHeading(
      env: Env,
      actionName: String,
      icon: Html[Nothing],
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]]
  ): Html[Nothing] =
    Html.h3( b.subtitle )(
      Html.span( va() )( actionName ),
      icon,
      Html.span( va() )(
        Html.text( if ( pos.direction == FlowEnd.Source ) "from" else "to" ),
        nbsp,
        RecipeFrag.splitName( env )( srcDest.item )
      )
    )

  private def actionButton( content: Elem[Nothing], classes: Classes, help: Elem[Nothing]* )(
      action: FlowAction,
      addon: Option[Html[FlowAction]],
      preview: Option[Html[Nothing]]
  ): Html[FlowAction] =
    Html.div( b.field )(
      Html.div( b.control )(
        Html.span( b.buttons + b.hasAddons, Html.style( CSS.display( "inline-flex" ) ) )(
          Html.button(
            b.button + classes,
            Html.onClick( action ),
            Option.when( preview.isEmpty )( Html.disabled( true ) )
          )( content ),
          addon
        ),
        preview.map( Html.span( b.ml2 )( _ ) )
      ),
      Html.p( b.help )( help* )
    )

  case class PreviewResultIcon( classes: Classes, icon: Classes, ifOverflow: Boolean ):
    def apply( preview: SplitMergePreview ): Option[Html[Nothing]] =
      Option.when( preview.overflow == ifOverflow ):
        Html.span( classes + b.ml2 )( Html.i( icon )() )

  object PreviewResultIcon:
    val WarnOnOverflow: PreviewResultIcon        = PreviewResultIcon( b.hasTextWarning, p.regular.warning, true )
    val SuccessUnlessOverflow: PreviewResultIcon = PreviewResultIcon( b.hasTextSuccess, p.regular.check, false )

  private def previewResult(
      icon: PreviewResultIcon,
      env: Env,
      item: Item,
      previewOpt: Option[SplitMergePreview]
  ): Option[Html[Nothing]] =
    previewOpt.map: preview =>
      Html.span(
        Html.span(
          preview.result
            .map: amount =>
              List[Elem[Nothing]]( RecipeFrag.numberedIconTag( env, Countable( item, amount ), b.mx2 ) )
            .intercalate( List[Elem[Nothing]]( Html.text( "+" ) ) )
        ),
        icon( preview )
      )

  def intActionParamSelector( param: IntActionParam ): Html[FlowAction] =
    Html.div( b.dropdown + b.isHoverable )(
      Html.div( b.dropdownTrigger )(
        Html.span( b.buttons + b.hasAddons )(
          Html.span(),
          Html.button( b.button )(
            Html.span( param.current.toString ),
            Html.span( b.icon + b.isSmall + b.hasTextLink )(
              Html.i( p.regular.caretDown )()
            )
          )
        )
      ),
      Html.div( b.dropdownMenu )(
        Html.div( b.dropdownContent )(
          ( param.min to param.max ).toList.map: n =>
            Html.div( b.dropdownItem, Html.onClick( param.action( n ) ) )(
              Html.span( n.toString ),
              Option.when( param.current == n )(
                Html.span( b.icon + b.isSmall + b.hasTextSuccess )(
                  Html.i( p.fill.checkFat )()
                )
              )
            )
        )
      )
    )

  case class IntActionParam(
      current: Int,
      min: Int,
      max: Int,
      action: Int => FlowAction
  )

  private def splitActionButton(
      flows: Flows,
      splitType: SplitType,
      pos: SrcDestPos,
      actionParam: Option[IntActionParam]
  )( content: Elem[Nothing], classes: Classes, help: Elem[Nothing] ): Html[FlowAction] =
    val splitPreviewOpt: Option[SplitMergePreview] = flows.previewSplit( pos, splitType )
    val splitShardsWarning: List[Html[Nothing]]    =
      splitPreviewOpt
        .filter( preview => preview.originalBoostedMachineCount.exists( _ < preview.result.size ) )
        .foldMap: _ =>
          List(
            Html.br(),
            Html.span( b.hasTextWarning )( "Will increase the number of somersloop used." )
          )
    actionButton( content, classes, help :: splitShardsWarning* )(
      FlowAction.SplitSrcDest( pos, splitType ),
      actionParam.map( intActionParamSelector ),
      previewResult(
        PreviewResultIcon.SuccessUnlessOverflow,
        flows.prod.env,
        pos.item,
        splitPreviewOpt
      )
    )

  private def equalSplitActionParam( current: Int ): IntActionParam =
    IntActionParam( current, 2, 10, FlowAction.SplitEqualSetCount( _ ) )
  private def byMachineActionParam( current: Int, max: Int ): IntActionParam =
    IntActionParam( current, 1, max, FlowAction.SplitByMachineSetCount( _ ) )

  private def splitSrcDestModal(
      flows: Flows,
      action: ActionModal.SplitAction
  ): Html[PlanMsg] =
    val env: Env = flows.prod.env
    Modal
      .apply( FlowAction.AbortModalFlowOp, Html.style( CSS.width( "60rem" ) ) )(
        Html.div( b.box )(
          splitModalHeading( env, action.pos, action.srcDest ),
          splitActionButton( flows, action.even, action.pos, none )(
            Html.text( "Evenly" ),
            b.isInfo,
            Html.text(
              s"spread the ${Numbers.showDouble3( action.srcDest.amount )} " +
                s"${action.pos.item.displayName} with equal headroom/overflow"
            )
          ),
          splitActionButton(
            flows,
            action.equal,
            action.pos,
            action.equalSplitCount.map( equalSplitActionParam )
          )(
            Html.text( "Equally" ),
            b.isInfo,
            Html.text(
              s"spread the ${Numbers.showDouble3( action.srcDest.amount )} ${action.pos.item.displayName} equally"
            )
          ),
          splitActionButton(
            flows,
            action.equalFixed,
            action.pos,
            action.equalSplitCount.map( equalSplitActionParam )
          )(
            Html.text( "Equally (fixed)" ),
            b.isInfo,
            Html.text(
              s"spread the ${Numbers.showDouble3( action.srcDest.amount )} ${action.pos.item.displayName} " +
                s"almost equally (without changing the number and clock speed of machines)"
            )
          ),
          splitActionButton(
            flows,
            action.byMachineCount,
            action.pos,
            action.machineCount.map( byMachineActionParam.tupled )
          )(
            Html.text( "By machine count" ),
            b.isInfo,
            Html.text( s"split a number of machines, keeping the same clock speed." )
          ),
          splitActionButton( flows, action.remainder, action.pos, none )(
            Html.text( "Remainder" ),
            b.isInfo,
            Html.text( s"split the amount that goes over the total amount on the other end." )
          ),
          Html.hr(),
          splitActionButton( flows, action.max, action.pos, none )(
            Html.text( "Max (once)" ),
            b.isPrimary,
            Html.text( s"split one max (${action.transport.perMinute}) flow from the remainder" )
          ),
          splitActionButton( flows, action.maxAll, action.pos, none )(
            Html.text( "Max (all)" ),
            b.isPrimary,
            Html.text( s"split as many max (${action.transport.perMinute}) flow as possible from the remainder" )
          ),
          Html.hr(),
          Html.div(
            action.opposite.map: splitType =>
              splitActionButton( flows, splitType, action.pos, none )(
                Html.span(
                  Html.text(
                    s"split ${Numbers.showDouble3( splitType.split.amount )} for"
                  ),
                  nbsp,
                  RecipeFrag.splitName( env )( splitType.split.item )
                ),
                b.isLink,
                none
              )
          )
        )
      )
      .map( PlanMsg.Flow( _ ) )

  private def mergeActionButton(
      flows: Flows,
      mergeType: MergeType,
      pos: SrcDestPos
  )( content: Elem[Nothing], classes: Classes, help: Elem[Nothing] ): Html[FlowAction] =
    actionButton( content, classes, help )(
      FlowAction.MergeSrcDest( pos, mergeType ),
      none,
      previewResult( PreviewResultIcon.WarnOnOverflow, flows.prod.env, pos.item, flows.previewMerge( pos, mergeType ) )
    )

  private def mergeSrcDestModal(
      flows: Flows,
      action: ActionModal.MergeAction
  ): Html[PlanMsg] =
    val env: Env = flows.prod.env
    Modal
      .apply( FlowAction.AbortModalFlowOp, Html.style( CSS.width( "60rem" ) ) )(
        Html.div( b.box )(
          mergeModalHeading( env, action.pos, action.srcDest ),
          mergeActionButton( flows, action.local, action.pos )(
            Html.text( "Local" ),
            b.isInfo,
            Html.span(
              Html.text( "Merge all" ),
              nbsp,
              RecipeFrag.srcDestName( env )( action.srcDest.item.original ),
              nbsp,
              Html.text( s"in ${action.transport.displayName} #${action.pos.index + 1}" )
            )
          ),
          mergeActionButton( flows, action.global, action.pos )(
            Html.text( "Global" ),
            b.isInfo,
            Html.span(
              Html.text( "Merge all" ),
              nbsp,
              RecipeFrag.srcDestName( env )( action.srcDest.item.original )
            )
          ),
          Html.hr(),
          Html.div(
            action.adjacent.map: mergeType =>
              mergeActionButton( flows, mergeType, action.pos )(
                Html.span(
                  Html.text( "Merge with" ),
                  nbsp,
                  RecipeFrag.splitName( env )( mergeType.split.item )
                ),
                b.isLink,
                none
              )
          )
        )
      )
      .map( PlanMsg.Flow( _ ) )

  def splitTransportModalTargetButton(
      flows: Flows,
      action: ActionModal.SplitTransportAction,
      target: Int
  ): Option[Html[FlowAction]] =
    flows.itemTransports
      .get( action.item )
      .flatMap( _.get( target ) )
      .map: itemTransport =>
        val transportName: String           = itemTransport.transport.displayName
        val transportAmounts: Html[Nothing] =
          val ( text, amount ) =
            action.flowEnd match
              case FlowEnd.Source      => ( "deficit of", itemTransport.destinationAmount - itemTransport.sourceAmount )
              case FlowEnd.Destination => ( "excess of", itemTransport.sourceAmount - itemTransport.destinationAmount )
          Html.span( b.ml2, va( "baseline" ) )(
            Html.text( s"with a $text" ),
            RecipeFrag.numberedIcon3( flows.prod.env, Countable( action.item, amount ), b.ml2, va( "baseline" ) )
          )

        Html.div( b.field )(
          Html.div( b.control )(
            Html.button(
              b.button + b.isLink,
              va( "baseline" ),
              Html.onClick(
                FlowAction.SplitTransport( action.item, action.index, action.flowEnd, target, action.amount )
              )
            )( s"Transport $transportName #${target + 1}" ),
            transportAmounts
          )
        )

  private def splitTransportModal(
      flows: Flows,
      action: ActionModal.SplitTransportAction
  ): Html[PlanMsg] =
    val actionName: String = action.flowEnd match
      case FlowEnd.Source      => "Split"
      case FlowEnd.Destination => "Merge"

    val actionPrep: String = action.flowEnd match
      case FlowEnd.Source      => "to"
      case FlowEnd.Destination => "from"

    Modal
      .apply( FlowAction.AbortModalFlowOp )(
        Html.div( b.box )(
          Html.h3( b.subtitle )(
            Html.span( va() )( actionName ),
            RecipeFrag.numberedIcon3( flows.prod.env, Countable( action.item, action.amount ), b.mx2 ),
            Html.span( va() )( actionPrep )
          ) ::
            action.targets.toList.mapFilter( splitTransportModalTargetButton( flows, action, _ ) )
        )
      )
      .map( PlanMsg.Flow( _ ) )
