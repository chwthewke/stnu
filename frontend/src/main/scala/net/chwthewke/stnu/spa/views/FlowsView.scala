package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.syntax.all.*
import tyrian.CSS
import tyrian.Elem
import tyrian.Html

import data.Countable
import model.Item
import model.Transport
import model.prod.FlowEnd
import spa.css.Bulma
import spa.css.Classes
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
        flows.itemFlows.values.foldLeft( ( false, false ) ):
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
        _.itemFlows.toList
          .mapFilter:
            case ( itemClass, itemTransports ) =>
              model.env.getItem( itemClass ).tupleRight( itemTransports )
          .sortBy( _._1.displayName )
    val groups: Groups = flows.foldMap( Groups.of )

    flows.flatMap( modal ) ++:
      PlanHeader(
        model,
        panelButtons :+ reset :+ toggleShow( model.productionUi )*
      ) ++:
      problemNotifications( model.env, itemFlows ) :++
      flows.map: f =>
        Html.div(
          itemFlows
            .filter:
              case ( _, trs ) =>
                model.productionUi.showAllFlows || trs.exists( it => it.overflow || !it.balanced )
            .map:
              case ( item, transports ) =>
                displayItem( f, groups, item, transports )
        )

  private def problemNotifications(
      env: Env,
      itemFlows: List[( Item, NonEmptyVector[ItemTransport] )]
  ): List[Html[Nothing]] =
    val ( overflow, unbalanced ) =
      itemFlows
        .foldMap:
          case ( item, itemTransports ) =>
            (
              Option.when( itemTransports.exists( _.overflow ) )( item ).toList,
              Option.when( itemTransports.exists( !_.balanced ) )( item ).toList
            )
    def notification( classes: Classes, text: String, items: NonEmptyList[Item] ): Html[Nothing] =
      Html.div( b.notification + classes )(
        Html.p( b.hasTextWeightBold + b.isSize4 )( s"$text:" ),
        Html.p(
          items.toList.map: item =>
            Html.span( b.tag + b.isDark + b.mx1 )(
              Html.span( b.iconText )( item.displayName ),
              Html.span( b.icon )( icon.verticalAlign().withDropShadow().item( env, item ) )
            )
        )
      )

    List(
      overflow.toNel
        .map( notification( b.isWarning, "The following items have overflows", _ ) ),
      unbalanced.toNel
        .map( notification( b.isDanger, "The following items are unbalanced", _ ) )
    ).flattenOption

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
        Html.td(
          if ( srcDest.amount > transport.perMinute * ( 1d + Countable.Tolerance ) )
            Html.i( va(), p.fill.warning + b.hasTextWarning )()
          else
            Html.i( Html.style( CSS.paddingRight( "16px" ) ) )()
        ),
        Html.td(
          Html
            .span( b.buttons + b.hasAddons )( flowActionButtons( flows, item, direction, transport, index, subIndex ) )
        ),
        Html.td( b.hasTextRight )( Html.strong( Numbers.showDouble3( srcDest.amount ) ) ),
        Html.td( b.hasTextCentered )(
          if ( srcDest.item.max == 1 ) "*" else s"#${srcDest.item.split}/${srcDest.item.max}"
        ),
        Html.td( srcDest.item.value.displayName ),
        Html.td( b.hasTextCentered )( renderSrcDestMachines( env, srcDest.item ) ),
        Html.td( b.hasTextRight )( FlowElements.groupButton( groups, srcDest.item.group, newGroup = false, none ) )
      )
      .map( PlanMsg.Flow( _ ) )

  private def flowTable(
      flows: Flows,
      groups: Groups,
      item: Item,
      direction: FlowEnd,
      transport: Transport,
      index: Int,
      srcDests: Vector[Countable[Double, Split[SrcDest]]]
  ): Html[PlanMsg] =
    Html.table( b.table + b.isFullwidth )( Html.tbody( srcDests.toList.zipWithIndex.map:
      case ( srcDest, ix ) => flowRow( flows, groups, item, direction, transport, index, ix, srcDest ) ) )

  private def displayItemTransport(
      flows: Flows,
      groups: Groups,
      item: Item,
      itemTransport: ItemTransport,
      index: Int
  ): Html[PlanMsg] =
    val env: Env = flows.prod.env
    Html.div( b.box )(
      FlowElements.transportHeader( env, itemTransport, index.some, warnings = true ),
      Html.div( b.columns )(
        Html.div( b.column )(
          Html.p( b.notification + b.isSuccess + b.isDark )(
            flowTable(
              flows,
              groups,
              item,
              FlowEnd.Source,
              itemTransport.transport.item,
              index,
              itemTransport.sources
            )
          )
        ),
        Html.div( b.column )(
          Html.p( b.notification + b.isInfo + b.isDark )(
            flowTable(
              flows,
              groups,
              item,
              FlowEnd.Destination,
              itemTransport.transport.item,
              index,
              itemTransport.destinations
            )
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
    Html.div( b.block )(
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
      case am: ActionModal.SplitAction => splitSrcDestModal( flows, am )
      case am: ActionModal.MergeAction => mergeSrcDestModal( flows, am )

  private def splitModalHeading(
      env: Env,
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]]
  ): Html[Nothing] =
    modalHeading(
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
      "Merge",
      Icons.icon.withClasses( b.mx2 ).verticalAlign().withDropShadow().item( env, pos.item ),
      pos,
      srcDest
    )

  private def modalHeading(
      actionName: String,
      icon: Html[Nothing],
      pos: SrcDestPos,
      srcDest: Countable[Double, Split[SrcDest]]
  ): Html[Nothing] =
    Html.h3( b.subtitle )(
      Html.span( va() )( actionName ),
      icon,
      Html.span( va() )( s"${if ( pos.direction == FlowEnd.Source ) "from" else "to"} ${srcDest.item.displayName}" )
    )

  private def actionButton( text: String, classes: Classes, help: String )(
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
          )( text ),
          addon
        ),
        preview.map( Html.span( b.ml2 )( _ ) )
      ),
      Html.p( b.help )( help )
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
  )( text: String, classes: Classes, help: String ): Html[FlowAction] =
    actionButton( text, classes, help )(
      FlowAction.SplitSrcDest( pos, splitType ),
      actionParam.map( intActionParamSelector ),
      previewResult(
        PreviewResultIcon.SuccessUnlessOverflow,
        flows.prod.env,
        pos.item,
        flows.previewSplit( pos, splitType )
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
      .apply( FlowAction.AbortScrDestOp )(
        Html.div( b.box )(
          splitModalHeading( env, action.pos, action.srcDest ),
          splitActionButton( flows, action.even, action.pos, none )(
            "Evenly",
            b.isInfo,
            s"spread the ${Numbers.showDouble3( action.srcDest.amount )} " +
              s"${action.pos.item.displayName} with equal headroom/overflow"
          ),
          splitActionButton(
            flows,
            action.equal,
            action.pos,
            action.equalSplitCount.map( equalSplitActionParam )
          )(
            "Equally",
            b.isInfo,
            s"spread the ${Numbers.showDouble3( action.srcDest.amount )} ${action.pos.item.displayName} equally"
          ),
          splitActionButton(
            flows,
            action.equalFixed,
            action.pos,
            action.equalSplitCount.map( equalSplitActionParam )
          )(
            "Equally (fixed)",
            b.isInfo,
            s"spread the ${Numbers.showDouble3( action.srcDest.amount )} ${action.pos.item.displayName} " +
              s"almost equally (without changing the number and clock speed of machines)"
          ),
          splitActionButton(
            flows,
            action.byMachineCount,
            action.pos,
            action.machineCount.map( byMachineActionParam.tupled )
          )(
            "By machine count",
            b.isInfo,
            s"split a number of machines, keeping the same clock speed."
          ),
          splitActionButton( flows, action.remainder, action.pos, none )(
            "Remainder",
            b.isInfo,
            s"split the amount that goes over the total amount on the other end."
          ),
          Html.hr(),
          splitActionButton( flows, action.max, action.pos, none )(
            "Max (once)",
            b.isPrimary,
            s"split one max (${action.transport.perMinute}) flow from the remainder"
          ),
          splitActionButton( flows, action.maxAll, action.pos, none )(
            "Max (all)",
            b.isPrimary,
            s"split as many max (${action.transport.perMinute}) flow as possible from the remainder"
          ),
          Html.hr(),
          Html.div(
            action.opposite.map: splitType =>
              splitActionButton( flows, splitType, action.pos, none )(
                s"split ${Numbers.showDouble3( splitType.split.amount )} for ${splitType.split.item.displayName}",
                b.isLink,
                ""
              )
          )
        )
      )
      .map( PlanMsg.Flow( _ ) )

  private def mergeActionButton(
      flows: Flows,
      mergeType: MergeType,
      pos: SrcDestPos
  )( text: String, classes: Classes, help: String ): Html[FlowAction] =
    actionButton( text, classes, help )(
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
      .apply( FlowAction.AbortScrDestOp )(
        Html.div( b.box )(
          mergeModalHeading( env, action.pos, action.srcDest ),
          mergeActionButton( flows, action.local, action.pos )(
            "Local",
            b.isInfo,
            s"Merge all ${action.srcDest.item.original.displayName} in " +
              s"${action.transport.displayName} #${action.pos.index + 1}"
          ),
          mergeActionButton( flows, action.global, action.pos )(
            "Global",
            b.isInfo,
            s"Merge all ${action.srcDest.item.original.displayName}"
          ),
          Html.hr(),
          Html.div(
            action.adjacent.map: mergeType =>
              mergeActionButton( flows, mergeType, action.pos )(
                s"Merge with ${mergeType.split.item.displayName}",
                b.isLink,
                ""
              )
          )
        )
      )
      .map( PlanMsg.Flow( _ ) )
