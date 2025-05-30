package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyList
import cats.syntax.all.*
import tyrian.Attribute
import tyrian.CSS
import tyrian.Html

import data.Countable
import model.Form
import model.Item
import model.Recipe
import model.Transport
import protocol.solver.SolverResponse
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.LogisticsOptions
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.RequestSelectionAction
import spa.plan.RequestSelectionModel
import spa.prod.ClockedRecipe

object PlanTable:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( model: PlanModel ): Html[PlanMsg] =
    Html.div(
      Html.table( b.table + b.isResponsive + b.isFullwidth, Html.style( CSS.tableLayout( "fixed" ) ) )(
        Html.colgroup(
          Html.col( Html.style( CSS.width( "8%" ) ) ),
          Html.col( Html.style( CSS.width( "3%" ) ) ),
          Html.col( Html.style( CSS.width( "20%" ) ) ),
          Html.col( Html.style( CSS.width( "25%" ) ) ),
          Html.col( Html.style( CSS.width( "5%" ) ) ),
          Html.col( Html.style( CSS.width( "15%" ) ) ),
          Html.col( Html.style( CSS.width( "8%" ) ) ),
          Html.col( Html.style( CSS.width( "8%" ) ) ),
          Html.col( Html.style( CSS.width( "8%" ) ) )
        ),
        Html.thead(
          Html.tr(
            Html.th( Html.style( CSS.textAlign( "center" ) ), Html.colspan := "2" )( "Amount" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "Item" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "Recipe" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ), Html.colspan := "3" )( "Machines" ), // 30 (5, 15, 10)
            Html.th( Html.style( CSS.textAlign( "center" ) ), Html.colspan := "2" )( "Transport" ) // 10 (5, 5)
          ),
          Html.tr(
            Html.th( Html.colspan := "4" )(),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "#" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "type" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "clock" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "#" ),
            Html.th( Html.style( CSS.textAlign( "center" ) ) )( "type" )
          )
        ),
        Html.tbody( ghostRows( model ) ++ computedRows( model ) )
      )
    )

  val ghostBorderStyle: String = "medium dashed"

  // rows for the requested items that were not requested in the last computed plan
  private def ghostRows( model: PlanModel ): List[Html[PlanMsg]] =
    val requestsToCompute: List[Countable[Double, ClassName[Item]]] = (
      model.solution.foldMap( _.requested.requested ).map( _.mapAmount( -_ ) ).toList
        ++ model.requestSelection.requested
    ).gather
      .filter( _.isSignificant )

    uncomputedItemsTable( model, requestsToCompute )(
      Html.div( b.notification + b.isInfo )(
        Html.p( "These requests have not yet been computed." ),
        Html.p( Html.text( "Press " ), Html.strong( "Compute" ), Html.text( " to recompute the plan." ) )
      )
    )

  private def computedRows( model: PlanModel ): List[Html[PlanMsg]] =
    model.solution.foldMap: solution =>
      solution.response match
        case err: SolverResponse.Error                  => errorRow( err ) :: Nil
        case SolverResponse.Solution( inputs, recipes ) =>
          val inputRows: List[Html[PlanMsg]] =
            uncomputedItemsTable( model, inputs.toList )(
              Html.div( b.notification + b.isWarning )(
                Html.p( "Input presentation TBD." )
              )
            )
          val productionRows =
            recipes
              .filter( _.isSignificant )
              .mapFilter:
                _.traverse: rc =>
                  model.env.game.recipes
                    .get( rc )
                    .collect:
                      case r: Recipe.Manufacturing => r
                .map( ClockedRecipe.roundUp )
              .map: ( process: ClockedRecipe ) =>
                val initCells             = amountItemCells( model.env, model.requestSelection, process.mainProduct )
                val ( trAmtCell, trCell ) = logisticsCells( model, process )
                Html.tr( Html.style( CSS.verticalAlign( "middle" ) ) )(
                  initCells
                    ++ List(
                      Html.td( Html.title := process.recipe.describe )( process.recipe.displayName ),
                      Html.td( b.isFamilyMonospace, Html.style( CSS.textAlign( "right" ) ) )(
                        process.machineCount.toString
                      ),
                      Html.td(
                        icon.verticalAlign().machine( model.env, process.machine ),
                        nbsp,
                        Html.text( process.machine.displayName )
                      ),
                      Html.td( b.isFamilyMonospace, Html.style( CSS.textAlign( "right" ) ) )(
                        show"${process.clockSpeed} %"
                      ),
                      trAmtCell,
                      trCell
                    )
                )

          inputRows ++ productionRows

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

  private def logisticsCells(
      model: PlanModel,
      process: ClockedRecipe
  ): ( Html[Nothing], Html[Nothing] ) =
    val transport = selectLogistics( model.env, model.logisticsOptions, process.mainProduct )

    (
      Html.td( b.isFamilyMonospace, Html.style( CSS.textAlign( "right" ) ) )(
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

  private def errorRow( error: SolverResponse.Error ): Html[Nothing] =
    Html.tr(
      Html.div( b.notification + b.isDanger )(
        error match
          case SolverResponse.InvalidModelVersion => "Invalid model version. Try clearing the cache and reloading."
          case SolverResponse.InvalidClasses( classes ) =>
            classes.mkString_( "Unknown classes: ", ", ", ". Try clearing the cache and reloading." )
          case SolverResponse.SolverError( message ) =>
            show"Solver error $message. Try adding recipes, lowering amounts or increasing resource nodes."
      )
    )

  private def uncomputedItemsTable(
      model: PlanModel,
      items: List[Countable[Double, ClassName[Item]]]
  )(
      notification: Html[PlanMsg]
  ): List[Html[PlanMsg]] = {
    val knownItems = items.mapFilter( _.traverse( model.env.game.items.get ) )
    knownItems
      .map( amountItemCells( model.env, model.requestSelection, _ ) )
      .zipWithIndex
      .map:
        case ( initCells, idx ) =>
          Html.tr(
            Html.styles(
              CSS.verticalAlign( "middle" ) ::
                Option.when( idx == 0 )( CSS.borderTop( ghostBorderStyle ) ) ++:
                Option.when( idx == knownItems.size - 1 )( CSS.borderBottom( ghostBorderStyle ) ) ++:
                List( CSS.borderLeft( ghostBorderStyle ), CSS.borderRight( ghostBorderStyle ) )*
            )
          )(
            initCells ++
              Option.when( idx == 0 )(
                Html.td(
                  Html.colspan := "6",
                  Html.rowspan := knownItems.size.toString,
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
      model: RequestSelectionModel,
      item: Countable[Double, Item]
  ): List[Html[PlanMsg]] =

    def editButton: Option[Html[PlanMsg]] =
      Option
        .when( model.requested.exists( _.item == item.item.className ) ):
          Html
            .button(
              b.button + b.ml2 + b.isSmall + b.isPrimary,
              Html.onClick( RequestSelectionAction.EditAmountStart( item.item ) )
            )( Html.i( p.regular.`pencil` )() )
            .map( PlanMsg.RequestSelection( _ ) )

    val editAndAmount: List[Html[PlanMsg]] =
      model.requestAmountEditor
        .filter( _._1.className == item.item.className )
        .fold(
          Html.td( b.isFamilyMonospace, Html.style( CSS.textAlign( "right" ) ) )(
            Html.text( Numbers.showDouble3( item.amount ) )
          )
            :: Html.td( b.px0, Html.style( CSS.textAlign( "center" ) ) )( editButton )
            :: Nil
        ):
          case ( _, input ) =>
            Html.td( b.px0, Html.style( CSS.textAlign( "right" ) ), Html.colspan := "2" )(
              requestAmountEditor( input )
            ) :: Nil

    editAndAmount :+
      Html.td()( icon.verticalAlign().item( env, item.item ), nbsp, Html.text( item.item.displayName ) )
