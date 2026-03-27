package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import tyrian.Attr
import tyrian.CSS
import tyrian.EmptyAttribute
import tyrian.Html
import tyrian.Style

import data.Countable
import model.Footprint
import model.Item
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.ProcessSplitId
import spa.css.Bulma
import spa.css.Classes
import spa.css.CssClass
import spa.css.Phosphor
import spa.plan.PlanMsg
import spa.prod.ClockedRecipe
import spa.prod.EndId
import spa.prod.Flows
import spa.prod.Groups
import spa.prod.ItemTransport
import spa.prod.ProdModel
import spa.prod.Split
import spa.prod.SrcDest

object PlanTable:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( ui: ProdModel.Ui, flows: Flows ): Html[PlanMsg] =
    Html.div(
      productionSummary( ui, flows.prod ),
      planTable( ui, flows )
    )

  private def productionSummary( ui: ProdModel.Ui, production: ProdModel ): Html[Nothing] =
    Details
      .open( isOpen = ui.productionSummaryExpanded )(
        Html.text( "Summary" ),
        Html.div( b.mx4 + b.columns )(
          Html.div( b.column + b.isHalf + b.hasTextCentered )(
            Html.p( b.isSize5 )( "Resources" ),
            Html.ul()(
              production.extractionRows
                .foldMap( cr => cr.productsPerMinute )
                .gather
                .map: ci =>
                  Html.li( b.hasTextWeightBold )(
                    RecipeFrag.numberedIcon1( production.env, ci, b.mr2 ),
                    Html.text( ci.item.displayName )
                  )
            )
          ),
          Html.div( b.column + b.isHalf + b.hasTextCentered )(
            Html.p( b.isSize5 )( "Machines" ),
            Html.ul(
              production.productionRows
                .foldMap( cr => SortedMap( cr.recipe.producedIn -> cr.machineCount ) )
                .toList
                .map:
                  case ( machine, amount ) =>
                    Html.li( b.hasTextWeightBold )(
                      RecipeFrag.numberedIcon1( production.env, Countable( machine, amount ), b.mr2 ),
                      Html.text( machine.displayName )
                    )
            )
          )
        )
      )

  private def planTable( ui: ProdModel.Ui, flows: Flows ): Html[PlanMsg] =
    Html.table( b.table + b.isResponsive + b.isFullwidth + b.isHoverable, Html.style( CSS.tableLayout( "fixed" ) ) )(
      // TODO at some point we're gonna have to do something better then fixed percentages
      Html.colgroup(
        Html.col( Html.style( CSS.width( "5%" ) ) ),
        Html.col( Html.style( CSS.width( "10%" ) ) ),
        Html.col( Html.style( CSS.width( "8%" ) ) ),
        Html.col( Html.style( CSS.width( "14%" ) ) ),
        Html.col( Html.style( CSS.width( "24%" ) ) ),
        Html.col( Html.style( CSS.width( "5%" ) ) ),
        Html.col( Html.style( CSS.width( "12%" ) ) ),
        Html.col( Html.style( CSS.width( "10%" ) ) ),
        Html.col( Html.style( CSS.width( "7%" ) ) ),
        Html.col( Html.style( CSS.width( "5%" ) ) )
      ),
      Html.thead(
        Html.tr(
          Html.th( Html.colspan := "2" )( "Group" ),
          Html.th( b.hasTextCentered )( "Amount" ),
          Html.th( b.hasTextCentered )( "Item" ),
          Html.th( b.hasTextCentered )( "Recipe" ),
          Html.th( b.hasTextCentered, Html.colspan := "3" )( "Machines" ), // 27 (5, 12, 10)
          Html.th( b.hasTextCentered, Html.colspan := "2" )( "Power" ) // 12 (7, 5)
        ),
        Html.tr(
          Html.th( Html.colspan := "5" )(),
          Html.th( b.hasTextCentered )( "#" ),
          Html.th( b.hasTextCentered )( "type" ),
          Html.th( b.hasTextCentered )( "clock" ),
          powerHeaderCell( flows.prod ),
          Html.th( b.hasTextWeightBold )( "MW" )
        )
      ),
      Html.tbody(
        ghostRows( flows.prod ) ++ extraInputRows( flows.prod ) ++ computedRows( ui, flows )
      )
    )

  private val ghostBorderStyle: String = "medium dashed"

  // rows for the requested items that were not requested in the last computed plan
  private def ghostRows( production: ProdModel ): List[Html[PlanMsg]] =
    uncomputedItemsTable( production.env, production.dirtyRequestRows )(
      Html.div( b.notification + b.isInfo )(
        Html.p( "These requests have not yet been computed." ),
        Html.p( Html.text( "Press " ), Html.strong( "Compute" ), Html.text( " to recompute the plan." ) )
      )
    )

  private def extraInputRows( production: ProdModel ): List[Html[PlanMsg]] =
    uncomputedItemsTable( production.env, production.otherInputs )(
      Html.div( b.notification + b.isWarning )(
        Html.p( "These inputs are not being produced." ),
        Html.p( Html.text( "Press " ), Html.strong( "Compute" ), Html.text( " to recompute the plan." ) )
      )
    )

  def groupedOrderedSplits(
      ui: ProdModel.Ui,
      flows: Flows
  ): ( Vector[ProdModel.Row], List[( Group, NonEmptyList[( ProdModel.Row, Int )] )] ) =
    val rows: List[ProdModel.Row]        = ProdModel.Ui.initRowOrder[List]( flows )
    val orderedRows: List[ProdModel.Row] =
      rows.sortBy( row => ( row.group, ui.rowOrder.foldMap( _.indexOf( row.splitId ) ), row.splitId ) )
    (
      orderedRows.toVector,
      orderedRows.zipWithIndex
        .groupByNel( _._1.group )
        .toList
    )

  private def tableRowsOf( ui: ProdModel.Ui, flows: Flows, rows: Vector[ProdModel.Row], groups: Groups )(
      row: ProdModel.Row,
      rowIndex: Int
  ): List[Html[PlanMsg]] =
    val expanded: Boolean             = ui.expandedRows( row.splitId )
    val complete: Boolean             = ui.completed( row.splitId )
    val moreRows: List[Html[Nothing]] =
      if ( expanded )
        expandedProcessRows( flows, row.splitId, row.process )
      else Nil
    mainComputedRow( flows.prod, rows, rowIndex, groups, row, expanded, complete ) :: moreRows

  private def computedRows( ui: ProdModel.Ui, flows: Flows ): List[Html[PlanMsg]] =
    flows.prod.solution.foldMap:
      case ProdModel.Solution.Failure( err ) => errorRow( err ) :: Nil
      case _: ProdModel.Solution.Result      =>
        val ( allRows, rowGroups ) = groupedOrderedSplits( ui, flows )
        val groups: Groups         = Groups.of( flows )

        rowGroups
          .foldLeft( ( Set.empty[Group], List.empty[Html[PlanMsg]] ) ):
            case ( ( summarizedGroups, acc ), ( group, rows ) ) =>
              val toSummarize: List[Group] =
                @tailrec
                def loop( acc: List[Group], curr: Group ): List[Group] =
                  curr.parent.filterNot( summarizedGroups.contains ) match
                    case None           => curr :: acc
                    case Some( parent ) => loop( curr :: acc, parent )
                loop( Nil, group )

              val tableRows: List[Html[PlanMsg]] =
                toSummarize.flatMap: g =>
                  groupSummary( ui, flows, groups, g, ui.expandedGroupSummaries( g ) )
                ++
                  rows.toList.flatMap:
                    case ( row, rowIndex ) =>
                      tableRowsOf( ui, flows, allRows, groups )( row, rowIndex )

              ( summarizedGroups ++ toSummarize, acc ++ tableRows )
          ._2

  private val noBorderCSS: Style           = CSS.borderBottom( "0" )
  private val noBorderStyle: Attr[Nothing] = Html.style( noBorderCSS )

  private def mainComputedRow(
      production: ProdModel,
      rows: Vector[ProdModel.Row],
      rowIndex: Int,
      groups: Groups,
      row: ProdModel.Row,
      expanded: Boolean,
      complete: Boolean
  ): Html[PlanMsg] =
    val process: ClockedRecipe  = row.process.times( row.fraction )
    val cellAttr: Attr[Nothing] =
      if ( expanded ) Html.style( noBorderCSS |+| vas() )
      else Html.style( vas() )
    val initCells: List[Html[PlanMsg]] =
      amountItemCells( production.env, process.mainProduct, cellAttr )

    Html.tr(
      Html.td( cellAttr )(
        groupRowDropdown( rowIndex, row.end, row.splitId, row.group, groups )
      ) ::
        Html.td( cellAttr )(
          Html.div( b.buttons + b.hasAddons, va() )(
            Elements.miniButtonWithMod( None, "Move row up, shift = x2, ctrl = x5", p.regular.arrowFatUp )( m =>
              PlanMsg.MoveProductionRow( rows, row.splitId, -m.mod )
            ),
            Elements.miniButtonWithMod( None, "Move row down, shift = x2, ctrl = x5", p.regular.arrowFatDown )( m =>
              PlanMsg.MoveProductionRow( rows, row.splitId, m.mod )
            ),
            Elements.miniButton( if ( complete ) b.hasTextSuccess else b.hasTextDark, "mark completed", p.fill.circle )(
              PlanMsg.ToggleMarkComplete( row.splitId ).some
            ),
            Elements.miniButton(
              None,
              if ( expanded ) "collapse" else "expand",
              if ( expanded ) p.regular.caretDown else p.regular.caretRight
            )( PlanMsg.ToggleProductionRowExpanded( row.splitId ).some )
          )
        ) ::
        initCells
        ++ List(
          Html.td(
            Html.title := process.recipe.describe,
            cellAttr
          )(
            process.recipe.displayName +
              Option.when( row.splitCount > 1 )( s" #${row.splitNumber}/${row.splitCount}" ).orEmpty
          ),
          Html.td( b.isFamilyMonospace + b.hasTextRight, cellAttr )(
            process.machineCount.toString
          ),
          Html.td( cellAttr )(
            icon.verticalAlign().machine( production.env, process.machine ),
            nbsp,
            Html.text( process.machine.displayName )
          ),
          Html.td( b.isFamilyMonospace + b.hasTextRight, cellAttr )(
            show"${process.clockSpeed} %"
          ),
          powerCell( process, cellAttr ),
          Html.td( cellAttr )( "MW" )
        )
    )

  private def groupsGrid( group: Group, endId: EndId, id: ProcessSplitId, groups: Groups ): Html[PlanMsg] =
    val groupCoords: NonEmptyVector[( Int, Int, Group, Boolean )] = // row, column, path, isNew
      def loop(
          rowOffset: Int,
          columnOffset: Int,
          prefix: Vector[Int],
          tail: Groups
      ): NonEmptyVector[( Int, Int, Group, Boolean )] =
        def thisGroup: ( Int, Int, Group, Boolean ) = ( rowOffset, columnOffset, Group( prefix ), false )
        tail match
          case Groups.Nil =>
            NonEmptyVector.one( thisGroup ) ++
              // NOTE disallows sub-group creation below some depth (8)
              Option.when( columnOffset < 7 )( ( rowOffset, columnOffset + 1, Group( prefix :+ 1 ), true ) ).toVector
          case Groups.SubGroups( children ) =>
            val childrenGrid: Vector[( Int, Int, Group, Boolean )] =
              1.to( children.lastKey + 1 )
                .toVector
                .foldLeft( ( 0, Vector.empty[( Int, Int, Group, Boolean )] ) ):
                  case ( ( childRowOffset, gridAcc ), childNum ) =>
                    val childOpt: Option[Groups] = children.get( childNum )
                    val offs: Int                = childOpt.fold( 1 )( _.widthWithNewSiblings )
                    val childrenElts: Vector[( Int, Int, Group, Boolean )] = childOpt match
                      case Some( child ) =>
                        loop( rowOffset + childRowOffset, columnOffset + 1, prefix :+ childNum, child ).toVector
                      case None =>
                        Vector( ( rowOffset + childRowOffset, columnOffset + 1, Group( prefix :+ childNum ), true ) )
                    ( childRowOffset + offs, gridAcc ++ childrenElts )
                ._2

            NonEmptyVector( thisGroup, childrenGrid )

      loop( 0, 0, Vector.empty, groups )

    gridTable(
      groupCoords.toVector.map:
        case ( r, c, g, n ) =>
          (
            r,
            c,
            FlowElements.groupButton(
              groups,
              g,
              n,
              PlanMsg.SetGroup( endId, id, g ).some,
              Option.when( g == group )( b.isDark )
            )
          )
    )

  private def gridTable[A]( elements: Vector[( Int, Int, Html[A] )] ): Html[A] =

    Html.table( b.table )(
      elements
        .groupByNev( _._1 )
        .foldMap: row =>
          List(
            Html.tr(
              row
                .foldLeft( ( 0, List.empty[Html[A]] ) ):
                  case ( ( columnOffset, acc ), ( _, col, elt ) ) =>
                    (
                      col + 1,
                      acc ++
                        Option
                          .when( col > columnOffset )(
                            Html.td( b.p1, noBorderStyle, Html.colspan := ( col - columnOffset ).toString )()
                          ) ++
                        List( Html.td( b.p1, noBorderStyle )( elt ) )
                    )
                ._2
            )
          )
    )

  private def groupDropdown[A](
      rowIndex: Option[Int],
      eltId: String,
      group: Group,
      groups: Groups
  )( content: Html[A]* ): Html[A] =
    val isUp: Boolean                       = rowIndex.exists( _ >= groups.widthWithNewSiblings )
    val dropdownDirection: Option[CssClass] = Option.when( isUp )( b.isUp )
    Html.div( b.dropdown + b.isHoverable + dropdownDirection )(
      Html.div( b.dropdownTrigger )( FlowElements.groupButton( groups, group, newGroup = false, none ) ),
      Html.div(
        b.dropdownMenu,
        Html.role := "menu",
        Html.id   := eltId,
        Html.styles(
          CSS.left( "34px" ),
          if ( isUp ) CSS.bottom( "-22px" ) else CSS.top( "-22px" ),
          CSS.maxHeight( "60vh" ),
          CSS.overflowY( "scroll" )
        )
      )(
        Html.div( b.dropdownContent, Html.style( CSS.border( "solid 1px gray" ) ) )(
          Html.div( b.dropdownItem )(
            content*
          )
        )
      )
    )

  private def groupRowDropdown(
      rowIndex: Int,
      endId: EndId,
      id: ProcessSplitId,
      group: Group,
      groups: Groups
  ): Html[PlanMsg] =
    groupDropdown( rowIndex.some, show"group$id", group, groups )(
      groupsGrid( group, endId, id, groups.closeTo( group ) )
    )

  def groupSummary(
      prodUi: ProdModel.Ui,
      flows: Flows,
      groups: Groups,
      group: Group,
      expanded: Boolean
  ): List[Html[PlanMsg]] =
    val cellAttr: Attr[Nothing] =
      if ( expanded ) Html.style( noBorderCSS |+| vas() )
      else Html.style( vas() )

    val flat: Boolean = !prodUi.detailedGroupSummaries( group )

    Html.tr(
      Html.onClick( PlanMsg.ToggleGroupSummaryExpanded( group ) )
    )(
      Html.td( cellAttr )(
        if ( group == Group.root )
          FlowElements.groupButton( groups, group, false, none )
        else
          summaryGroupDropdown( groups, group )
      ),
      Html.td( cellAttr, Html.colspan := "6" )(
        Html.strong( s"Summary for ${FlowElements.longGroupName( group )}." ),
        Html.span( b.ml2 + b.isSize7 )( s"Click row to ${if ( expanded ) "collapse" else "expand"}" )
      ),
      Html.td( cellAttr, Html.colspan := "3" )(
        Option.when( expanded )(
          Html.div( b.buttons + b.areSmall + b.hasAddons + b.isRight )(
            Html.button(
              b.button + Option.when( flat )( b.isLink ),
              Html.onClick( PlanMsg.ToggleGroupSummaryFlat( group ) )
            )( "flat" ),
            Html.button(
              b.button + Option.when( !flat )( b.isLink ),
              Html.onClick( PlanMsg.ToggleGroupSummaryFlat( group ) )
            )( "transports" )
          )
        )
      )
    ) ::
      Option
        .when( expanded )(
          Html.tr(
            Html.td( Html.colspan := "10" )(
              GroupSummary(
                flows,
                groups,
                group,
                flows.groupFlows( group ),
                flat
              )
            )
          )
        )
        .toList

  def summaryGroupDropdown( groups: Groups, group: Group ): Html[PlanMsg] =
    def sibling( direction: Int ): Option[Group] =
      group.path.toNev
        .map( nev => Group( nev.init :+ ( nev.last + direction ) ) )
        .filter( groups.hasSlot )

    def swapButton( target: Option[Group], icon: Classes ): Html[PlanMsg] =
      Elements.miniButton(
        b.isInfo + b.isOutlined,
        target.foldMap( s => s"Swap with ${FlowElements.longGroupName( s )}" ),
        icon,
        Html.disabled( target.isEmpty ),
        Html.style( FlowElements.groupHueStyle( groups, group ) )
      )( target.map( PlanMsg.SwapGroups( group, _ ) ) )

    groupDropdown(
      none,
      show"groupswap$group",
      group,
      groups
    )(
      Html.span( b.buttons + b.hasAddons )(
        swapButton( sibling( -1 ), p.regular.arrowFatUp ),
        swapButton( sibling( 1 ), p.regular.arrowFatDown )
      )
    )

  private def powerHeaderCell( model: ProdModel ): Html[Nothing] =
    val consumption: Double =
      model.productionRows
        .foldMap: cr =>
          cr.power.average
    Html.th(
      Html.title := f"$consumption%.6f",
      b.hasTextRight + b.hasTextWeightBold + b.isFamilyMonospace
    )( f"$consumption%.1f" )

  private def powerCell( process: ClockedRecipe, borderAttr: Attr[Nothing] ): Html[Nothing] =
    Html.td(
      Html.title := f"${process.power.average}%.6f",
      b.hasTextRight + b.isFamilyMonospace,
      borderAttr
    )( f"${process.power.average}%.1f" )

  private def errorRow( error: String ): Html[Nothing] =
    Html.tr( Html.td( Html.colspan := "9" )( Html.div( b.notification + b.isDanger )( error ) ) )

  private def uncomputedItemsTable(
      env: Env,
      items: List[Countable[Double, Item]]
  )(
      notification: Html[PlanMsg]
  ): List[Html[PlanMsg]] = {
    items
      .map( item => amountItemCells( env, Some( item ), EmptyAttribute ) )
      .zipWithIndex
      .map:
        case ( initCells, idx ) =>
          Html.tr(
            Html.styles(
              CSS.verticalAlign( "middle" ) ::
                Option.when( idx == 0 )( CSS.borderTop( ghostBorderStyle ) ) ++:
                Option.when( idx == items.size - 1 )( CSS.borderBottom( ghostBorderStyle ) ) ++:
                List( CSS.borderLeft( ghostBorderStyle ), CSS.borderRight( ghostBorderStyle ) )*
            )
          )(
            Html.td( Html.colspan := "2" )() ::
              initCells ++
              Option.when( idx == 0 )(
                Html.td(
                  Html.colspan := "6",
                  Html.rowspan := items.size.toString,
                  Html.styles( CSS.textAlign( "center" ), CSS.verticalAlign( "middle" ) )
                )( notification )
              )
          )
  }

  private def amountItemCells(
      env: Env,
      itemOpt: Option[Countable[Double, Item]],
      borderAttr: Attr[Nothing]
  ): List[Html[PlanMsg]] =
    List(
      Html.td( b.isFamilyMonospace + b.hasTextRight, borderAttr )(
        Html.text( Numbers.showDouble3( itemOpt.foldMap( _.amount ) ) )
      ),
      itemOpt
        .map: item =>
          Html.td( borderAttr )(
            icon.verticalAlign().item( env, item.item ),
            nbsp,
            Html.text( item.item.displayName )
          )
        .getOrElse( Html.td() )
    )

  def expandedProcess( flows: Flows, splitId: ProcessSplitId ): Html[Nothing] =
    def itemTransportOfSplit(
        item: Item,
        transport: ItemTransport,
        direction: FlowEnd
    ): Map[FlowEnd, Map[Item, ( Double, Vector[Countable[Double, Split[SrcDest]]] )]] =
      transport
        .getMachineFlows( direction.opposite )
        .find( _.item.id == splitId )
        .foldMap: ci =>
          Map( direction -> Map( item -> ( ci.amount, transport.getMachineFlows( direction ) ) ) )

    // Source -> ingredients
    // Destination -> products
    val itemTransports: Map[FlowEnd, Map[Item, ( Double, Vector[Countable[Double, Split[SrcDest]]] )]] =
      flows.itemTransports.toVector
        .foldMap:
          case ( itemClass, transports ) =>
            flows.prod.env
              .getItem( itemClass )
              .foldMap: item =>
                transports.foldMap: transport =>
                  FlowEnd.cases.foldMap: direction =>
                    itemTransportOfSplit( item, transport, direction )

    val ( ingrColW: Int, prodColW: Int ) =
      val ingredients = itemTransports.get( FlowEnd.Source ).foldMap( _.size )
      val products    = itemTransports.get( FlowEnd.Destination ).foldMap( _.size )
      val count       = ingredients + products
      if ( count == 0 ) ( 6, 6 )
      else ( math.round( 12d * ingredients / count ).toInt, math.round( 12d * products / count ).toInt )

    def peerTableRows(
        groups: Groups,
        item: Item,
        amount: Double,
        peers: Vector[Countable[Double, Split[SrcDest]]]
    ): List[Html[Nothing]] =
      val borderStyle = Html.styles(
        CSS.borderBottomColor( "var(--bulma-table-cell-border-color)" ),
        CSS.borderBottomStyle( "var(--bulma-table-cell-border-style)" ),
        CSS.borderBottomWidth( "2px" )
      )
      List(
        Html.thead(
          Html.tr(
            Html.th( borderStyle )(
              Html.span( b.isFamilyMonospace + b.hasTextWeightBold )( Numbers.showDouble3( amount ) )
            ),
            Html.th( borderStyle, b.hasTextCentered )( icon.verticalAlign().item( flows.prod.env, item ) ),
            Html.th( borderStyle )( Html.text( item.displayName ) )
          )
        ),
        Html.tbody(
          peers.toList.map: ci =>
            Html.tr(
              Html.td( b.isFamilyMonospace )( Numbers.showDouble3( ci.amount ) ),
              Html.td( FlowElements.groupButton( groups, ci.item.group, newGroup = false, action = none ) ),
              Html.td( ci.item.displayName )
            )
        )
      )

    def peerTables( groups: Groups, direction: FlowEnd ): Html[Nothing] =
      Html.div( b.columns + b.isVcentered + b.isMultiline )(
        itemTransports
          .get( direction )
          .foldMap( _.toList )
          .map:
            case ( item, ( amount, peers ) ) =>
              Html.div( b.column + b.isNarrow )(
                Html.table( b.table )(
                  peerTableRows( groups, item, amount, peers )
                )
              )
      )

    val groups: Groups = Groups.of( flows )

    Html.tr(
      Html.td( Html.colspan := "10" )(
        Html.div( b.columns )(
          Option.when( ingrColW > 0 )(
            Html.div( b.column + b.cls( s"is-$ingrColW" ) )(
              Elements.messageCenteredHeader( b.isSuccess, b.hasTextSuccessDark, Html.text( "Inputs" ) )(
                peerTables( groups, FlowEnd.Source )
              )
            )
          ),
          Html.div( b.column + b.cls( s"is-$prodColW" ) )(
            Elements.messageCenteredHeader( b.isInfo, b.hasTextInfoDark, Html.text( "Products" ) )(
              peerTables( groups, FlowEnd.Destination )
            )
          )
        )
      )
    )

  def processRecipe( env: Env, process: ClockedRecipe ): Html[Nothing] =
    Html.tr(
      Html.td( noBorderStyle, Html.colspan := "10", b.isSize5 + b.hasTextCentered )(
        RecipeFrag.recipeIcons( env )( process.recipe )
      )
    )

  def processFootprint( process: ClockedRecipe ): Option[Html[Nothing]] =
    process.recipe.producedIn.footprint.map: footprint =>
      def estimateFootprint( machineRows: Int ): Footprint =
        val width: Int  = footprint.width * ( 1 + ( process.machineCount - 1 ) / machineRows )
        val length: Int = ( footprint.length + 800 ) * machineRows - 800
        Footprint( length, width )
      val estimates: List[( Int, Footprint )] =
        1.to( process.machineCount.min( 4 ) ).toList.fproduct( estimateFootprint )

      Html.tr(
        Html.td( noBorderStyle, Html.colspan := "10", b.hasTextCentered )(
          Elements.messageCenteredHeader( b.isPrimary, b.hasTextPrimaryDark, Html.text( "Footprint estimates" ) )(
            Html.table( b.table + b.isFullwidth )(
              Html.thead(
                estimates._1F.map: n =>
                  Html.th( b.hasTextCentered )( s"$n row${Option.when( n > 1 )( "s" ).orEmpty}" )
              ),
              Html.tbody(
                estimates._2F.map: footprint =>
                  Html.td( b.hasTextCentered )( FootprintElements.text( footprint ) )
              )
            )
          )
        )
      )

  def expandedProcessRows( flows: Flows, splitId: ProcessSplitId, process: ClockedRecipe ): List[Html[Nothing]] =
    processRecipe( flows.prod.env, process )
      :: processFootprint( process )
      ++: List( expandedProcess( flows, splitId ) )
