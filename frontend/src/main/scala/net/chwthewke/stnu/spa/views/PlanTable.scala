package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import tyrian.Attr
import tyrian.Attribute
import tyrian.CSS
import tyrian.EmptyAttribute
import tyrian.Html
import tyrian.Style

import data.Countable
import model.Form
import model.Item
import model.Transport
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.LogisticsOptions
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.RequestSelectionAction
import spa.plan.RequestSelectionModel
import spa.prod.ClockedRecipe
import spa.prod.ItemIO
import spa.prod.ProdModel
import spa.prod.SrcDest

object PlanTable:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( ui: PlanModel.Ui, production: ProdModel ): Html[PlanMsg] =
    Html.div(
      productionSummary( ui, production ),
      Html.table( b.table + b.isResponsive + b.isFullwidth + b.isHoverable, Html.style( CSS.tableLayout( "fixed" ) ) )(
        Html.colgroup(
          Html.col( Html.style( CSS.width( "2%" ) ) ),
          Html.col( Html.style( CSS.width( "10%" ) ) ),
          Html.col( Html.style( CSS.width( "3%" ) ) ),
          Html.col( Html.style( CSS.width( "18%" ) ) ),
          Html.col( Html.style( CSS.width( "32%" ) ) ),
          Html.col( Html.style( CSS.width( "5%" ) ) ),
          Html.col( Html.style( CSS.width( "10%" ) ) ),
          Html.col( Html.style( CSS.width( "10%" ) ) ),
          Html.col( Html.style( CSS.width( "7%" ) ) ),
          Html.col( Html.style( CSS.width( "3%" ) ) )
        ),
        Html.thead(
          Html.tr(
            Html.th( b.hasTextCentered, Html.colspan := "3" )( "Amount" ),
            Html.th( b.hasTextCentered )( "Item" ),
            Html.th( b.hasTextCentered )( "Recipe" ),
            Html.th( b.hasTextCentered, Html.colspan := "3" )( "Machines" ), // 30 (5, 15, 10)
            Html.th( b.hasTextCentered, Html.colspan := "2" )( "Power" ) // 10 (5, 5)
          ),
          Html.tr(
            Html.th( Html.colspan := "5" )(),
            Html.th( b.hasTextCentered )( "#" ),
            Html.th( b.hasTextCentered )( "type" ),
            Html.th( b.hasTextCentered )( "clock" ),
            powerHeaderCell( production ),
            Html.th( b.hasTextWeightBold )( "MW" )
          )
        ),
        Html.tbody(
          ghostRows( production ) ++ extraInputRows( production ) ++ computedRows( production )
        )
      )
    )

  private def numberedItem( env: Env, ci: Countable[Double, Item] ) =
    Html.span( b.column + b.is2 )(
      Html.text( Numbers.showDouble3( ci.amount ) ),
      nbsp,
      icon.verticalAlign().item( env, ci.item ),
      nbsp,
      Html.text( ci.item.displayName )
    )

  private def productionSummary( ui: PlanModel.Ui, production: ProdModel ): Html[Nothing] =
    Details
      .open( isOpen = ui.productionSummaryExpanded )(
        Html.text( "Summary" ),
        Html.div( b.mx4 )(
          Html.p( "Resources" ),
          Html.div( b.columns + b.hasTextWeightBold + b.isMultiline )(
            production.extractionRows
              .foldMap( cr => cr.productsPerMinute )
              .gather
              .map: ci =>
                numberedItem( production.env, ci )
          ),
          Html.p( "Machines" ),
          Html.div( b.columns + b.hasTextWeightBold + b.isMultiline )(
            production.rows
              .foldMap( cr => SortedMap( cr.recipe.producedIn -> cr.machineCount ) )
              .toList
              .map:
                case ( machine, amount ) =>
                  Html.span( b.column )(
                    Html.text( amount.toString ),
                    nbsp,
                    icon.verticalAlign().machine( production.env, machine ),
                    nbsp,
                    Html.text( machine.displayName )
                  )
          ),
          Html.p( "Requested" ),
          Html.div( b.columns + b.hasTextWeightBold + b.isMultiline )(
            production.currentRequest.toList
              .sortBy: ci =>
                ( ci.item.tier, ci.item.displayName )
              .map: ci =>
                numberedItem( production.env, ci )
          )
        )
      )

  private val ghostBorderStyle: String = "medium dashed"

  // rows for the requested items that were not requested in the last computed plan
  private def ghostRows( production: ProdModel ): List[Html[PlanMsg]] =
    uncomputedItemsTable( production.env, production.requestSelection, production.dirtyRequestRows )(
      Html.div( b.notification + b.isInfo )(
        Html.p( "These requests have not yet been computed." ),
        Html.p( Html.text( "Press " ), Html.strong( "Compute" ), Html.text( " to recompute the plan." ) )
      )
    )

  private def extraInputRows( production: ProdModel ): List[Html[PlanMsg]] =
    uncomputedItemsTable( production.env, production.requestSelection, production.otherInputs )(
      Html.div( b.notification + b.isWarning )(
        Html.p( "These inputs are not being produced." ),
        Html.p( Html.text( "Press " ), Html.strong( "Compute" ), Html.text( " to recompute the plan." ) )
      )
    )

  private def computedRows( production: ProdModel ): List[Html[PlanMsg]] =
    production.solution.foldMap:
      case ProdModel.Solution.Failure( err ) => errorRow( err ) :: Nil
      case _: ProdModel.Solution.Result      =>
        production.rows
          .flatMap: ( process: ClockedRecipe ) =>
            val expanded: Boolean             = production.expandedRecipe.contains_( process.recipe.className )
            val moreRows: List[Html[Nothing]] =
              if ( expanded )
                expandedRecipeRows( production.env )( production.itemIO, process )
              else Nil
            mainComputedRow( production, process, expanded ) :: moreRows

  private val noBorderCSS: Style           = CSS.borderBottom( "0" )
  private val noBorderStyle: Attr[Nothing] = Html.style( noBorderCSS )

  private def mainComputedRow(
      production: ProdModel,
      process: ClockedRecipe,
      expanded: Boolean
  ): Html[PlanMsg] =
    val borderAttr: Attr[Nothing]      = Option.when( expanded )( noBorderStyle )
    val initCells: List[Html[PlanMsg]] =
      amountItemCells( production.env, production.requestSelection, process.mainProduct, borderAttr )

    Html.tr(
      Html.styles( CSS.verticalAlign( "middle" ) )
    )(
      Html.td( borderAttr )(
        Html.button(
          b.button + b.isSmall,
          Html.onClick( PlanMsg.ToggleProductionRowExpanded( process.recipe.className ) )
        )(
          Html.i(
            if ( expanded ) p.regular.caretDown else p.regular.caretRight
          )()
        )
      ) ::
        initCells
        ++ List(
          Html.td(
            Html.title := process.recipe.describe,
            borderAttr
          )( process.recipe.displayName ),
          Html.td( b.isFamilyMonospace + b.hasTextRight, borderAttr )(
            process.machineCount.toString
          ),
          Html.td( borderAttr )(
            icon.verticalAlign().machine( production.env, process.machine ),
            nbsp,
            Html.text( process.machine.displayName )
          ),
          Html.td( b.isFamilyMonospace + b.hasTextRight, borderAttr )(
            show"${process.clockSpeed} %"
          ),
          powerCell( process, borderAttr ),
          Html.td( noBorderStyle )( "MW" )
        )
    )
  private def selectLogistics(
      env: Env,
      options: LogisticsOptions,
      product: Countable[Double, Item]
  ): Countable[Double, Transport] =
    def select(
        allowedChoices: NonEmptyList[Transport],
        amount: Double
    ): Countable[Double, Transport] =
      val choice: Transport = allowedChoices.find( _.perMinute >= amount ).getOrElse( allowedChoices.last )
      Countable( choice, amount / choice.perMinute )

    product.item.form match
      case Form.Solid =>
        select( options.belts( env ), product.amount )
      case Form.Liquid | Form.Gas =>
        select( options.pipelines( env ), product.amount )

  def logisticsCells(
      model: PlanModel,
      process: ClockedRecipe
  ): ( Html[Nothing], Html[Nothing] ) =
    val transportOpt = process.mainProduct.map( selectLogistics( model.env, model.logisticsOptions, _ ) )

    transportOpt
      .map: transport =>
        (
          Html.td( b.isFamilyMonospace + b.hasTextRight )(
            Html.text( Numbers.showDouble1( transport.amount ) ),
            nbsp,
            Html.text( s"(${transport.amount.ceil.toInt.toString})" )
          ),
          Html.td(
            icon.verticalAlign().transport( model.env, transport.item ),
            nbsp,
            Html.text( transport.item.displayName.split( ' ' ).last )
          )
        )
      .getOrElse( ( Html.td(), Html.td() ) )

  def powerHeaderCell( model: ProdModel ): Html[Nothing] =
    val consumption: Double =
      model.rows
        .foldMap: cr =>
          cr.power.average
    Html.th(
      Html.title := f"$consumption%.6f",
      b.hasTextRight + b.hasTextWeightBold + b.isFamilyMonospace
    )( f"$consumption%.1f" )

  def powerCell( process: ClockedRecipe, borderAttr: Attr[Nothing] ): Html[Nothing] =
    Html.td(
      Html.title := f"${process.power.average}%.6f",
      b.hasTextRight + b.isFamilyMonospace,
      borderAttr
    )( f"${process.power.average}%.1f" )

  private def errorRow( error: String ): Html[Nothing] =
    Html.tr( Html.td( Html.colspan := "9" )( Html.div( b.notification + b.isDanger )( error ) ) )

  private def uncomputedItemsTable(
      env: Env,
      requestSelection: RequestSelectionModel,
      items: List[Countable[Double, Item]]
  )(
      notification: Html[PlanMsg]
  ): List[Html[PlanMsg]] = {
    items
      .map( item => amountItemCells( env, requestSelection, Some( item ), EmptyAttribute ) )
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
            Html.td() ::
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

  private def requestAmountEditor( input: InputModel ): Html[PlanMsg] =
    Html
      .div( b.field + b.hasAddons + b.hasAddonsRight )(
        Html.div( b.control )(
          Html
            .div(
              b.button + b.isSmall + b.isDanger,
              Html.style( CSS.height( "var(--bulma-control-height)" ) ),
              Html.onClick( RequestSelectionAction.EditAmountDelete )
            )(
              Html.i( p.regular.`trash` )()
            )
        ),
        Html.div( b.control )(
          Html
            .div(
              b.button + b.isSmall + b.isInfo,
              Html.style( CSS.height( "var(--bulma-control-height)" ) ),
              Html.onClick( RequestSelectionAction.EditAmountCancel )
            )(
              Html.i( p.regular.`arrowUUpLeft` )()
            )
        ),
        Html
          .div( b.control )(
            Html.input(
              b.input + b.isSmall,
              Html.style( CSS.width( "5em" ) ),
              Html.id     := RequestSelectionModel.editorId,
              Html.`type` := "text",
              Attribute( "inputmode", "decimal" ),
              input.output.map( v => Html.value := v ),
              Html.onInput( value => RequestSelectionAction.EditAmountSetValue( value ) )
            )
          ),
        Html
          .div( b.control )(
            Html.div(
              b.button + b.isSmall + b.isSuccess,
              Html.style( CSS.height( "var(--bulma-control-height)" ) ),
              Html.onClick( RequestSelectionAction.EditAmountCommit )
            )(
              Html.i( p.regular.check )()
            )
          )
      )
      .map( PlanMsg.RequestSelection( _ ) )

  private def amountItemCells(
      env: Env,
      requested: RequestSelectionModel,
      itemOpt: Option[Countable[Double, Item]],
      borderAttr: Attr[Nothing]
  ): List[Html[PlanMsg]] =

    def editButton: Option[Html[PlanMsg]] =
      itemOpt.flatMap: item =>
        Option
          .when( requested.requestedItems( item.item.className ) ):
            Html
              .button(
                b.button + b.ml2 + b.isSmall + b.isPrimary,
                Html.onClick( RequestSelectionAction.EditAmountStart( item.item.className ) )
              )( Html.i( p.regular.`pencil` )() )
              .map( PlanMsg.RequestSelection( _ ) )

    val editAndAmount: List[Html[PlanMsg]] =
      requested.requestAmountEditor
        .filter { case ( ed, _ ) => itemOpt.exists( _.item.className == ed ) }
        .fold(
          Html.td( b.isFamilyMonospace + b.hasTextRight, borderAttr )(
            Html.text( Numbers.showDouble3( itemOpt.foldMap( _.amount ) ) )
          )
            :: Html.td( b.px0 + b.hasTextCentered, borderAttr )( editButton )
            :: Nil
        ):
          case ( _, input ) =>
            Html.td( b.px0 + b.hasTextRight, borderAttr, Html.colspan := "2" )(
              requestAmountEditor( input )
            ) :: Nil

    editAndAmount :+
      itemOpt
        .map: item =>
          Html.td( borderAttr )( icon.verticalAlign().item( env, item.item ), nbsp, Html.text( item.item.displayName ) )
        .getOrElse( Html.td() )

  private def expandedRecipeRows(
      env: Env
  )( itemIO: Map[Item, ItemIO[SrcDest]], recipe: ClockedRecipe ): List[Html[Nothing]] =
    List(
      Html.tr( Html.style( CSS.borderTop( "0" ) ) )(
        Html.td( Html.colspan := "4", noBorderStyle )(),
        Html.td( noBorderStyle )( RecipeFrag.recipeIcons( env )( recipe.recipe ) ),
        Html.td( Html.colspan := "5", noBorderStyle )()
      ),
      Html.tr(
        Html.td( Html.colspan := "10" )(
          Html.div( b.columns )(
            Html.div( b.column + b.is6 )(
              Html.h4( b.subtitle + b.hasTextCentered )( "Inputs" ),
              Html.table( b.table, Html.style( CSS.margin( "0 auto" ) ) )(
                itemsIOTableRows( env, "FROM", _.sources )( itemIO, recipe.ingredientsPerMinute )
              )
            ),
            Html.div( b.column + b.is6 )(
              Html.h4( b.subtitle + b.hasTextCentered )( "Outputs" ),
              Html.table( b.table, Html.style( CSS.margin( "0 auto" ) ) )(
                itemsIOTableRows( env, "TO", _.destinations )( itemIO, recipe.productsPerMinute )
              )
            )
          )
        )
      )
    )

  private def itemsIOTableRows(
      env: Env,
      word: String,
      direction: ItemIO[SrcDest] => Vector[Countable[Double, SrcDest]]
  )(
      itemIO: Map[Item, ItemIO[SrcDest]],
      items: List[Countable[Double, Item]]
  ): List[Html[Nothing]] =
    items
      .mapFilter: ci =>
        itemIO.get( ci.item ).tupleLeft( ci )
      .foldMap:
        case ( item, itemIO ) =>
          itemIOTableRows( env, word )( item, direction( itemIO ) )

  private def itemIOTableRows( env: Env, word: String )(
      item: Countable[Double, Item],
      peers: Vector[Countable[Double, SrcDest]]
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
            Html.span( b.isFamilyMonospace + b.hasTextWeightBold )( Numbers.showDouble3( item.amount ) )
          ),
          Html.th( borderStyle, b.hasTextCentered )( icon.verticalAlign().item( env, item.item ) ),
          Html.th( borderStyle )( Html.text( item.item.displayName ) )
        )
      ),
      Html.tbody(
        peers.toList.mapFilter: ci =>
          val peerNameOpt: Option[String] =
            ci.item match
              case SrcDest.Extract( recipe ) => env.getRecipe( recipe ).map( _.displayName )
              case SrcDest.Step( recipe )    => env.getRecipe( recipe ).map( _.displayName )
              case SrcDest.Input             => "INPUT".some
              case SrcDest.Requested         => "REQUESTED".some
              case SrcDest.Byproduct         => "BYPRODUCT".some
          peerNameOpt.map: peerName =>
            Html.tr(
              Html.td( b.isFamilyMonospace )( Numbers.showDouble3( ci.amount ) ),
              Html.td( word ),
              Html.td( peerName )
            )
      )
    )
