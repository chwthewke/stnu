package net.chwthewke.stnu
package spa
package plan

import cats.effect.Async
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import tyrian.Cmd
import tyrian.cmds.Dom

import data.Countable
import model.Item

case class RequestSelectionModel(
    requestedAmounts: SortedMap[ClassName[Item], Double],
    search: SearchQuery,
    requestAmountEditor: Option[( ClassName[Item], InputModel )]
):
  val requestedItems: Set[ClassName[Item]] = requestedAmounts.keySet

  def update[F[_]: Async]( msg: RequestSelectionAction ): ( RequestSelectionModel, Cmd[F, Nothing] ) =
    msg match

      case RequestSelectionAction.SearchInput( value ) => copy( search = search.onInput( value ) ) -> Cmd.None
      case RequestSelectionAction.SearchReset          => copy( search = search.clear )            -> Cmd.None
      case RequestSelectionAction.RequestItem( item )  =>
        copy( requestedAmounts = requestedAmounts.updatedWith( item )( _.getOrElse( 1.0d ).some ) ) -> Cmd.None
      case RequestSelectionAction.EditAmountStart( item ) =>
        editRequestAmountStart( item )
      case RequestSelectionAction.EditAmountCancel =>
        editRequestAmountCancel -> Cmd.None
      case RequestSelectionAction.EditAmountCommit =>
        editRequestAmountCommit -> Cmd.None
      case RequestSelectionAction.EditAmountDelete =>
        editRequestAmountDelete -> Cmd.None
      case RequestSelectionAction.EditAmountSetValue( value ) =>
        editRequestAmountInput( value ) -> Cmd.None

  private def editRequestAmountStart[F[_]: Async]( item: ClassName[Item] ): ( RequestSelectionModel, Cmd[F, Nothing] ) =
    requestedAmounts
      .get( item )
      .fold( this -> Cmd.None ): amt =>
        copy( requestAmountEditor = Some( ( item, InputModel.withDefault( amt.toString ) ) ) ) ->
          Dom.focus( RequestSelectionModel.editorId )

  private def editRequestAmountDelete: RequestSelectionModel =
    requestAmountEditor.fold( this ):
      case ( item, _ ) =>
        copy( requestAmountEditor = none, requestedAmounts = requestedAmounts - item )

  private def editRequestAmountCancel: RequestSelectionModel =
    copy( requestAmountEditor = none )

  private def editRequestAmountInput( value: String ): RequestSelectionModel =
    copy( requestAmountEditor = requestAmountEditor.map:
      case ( item, _ ) => ( item, InputModel.onInput( value ) ) )

  private def editRequestAmountCommit: RequestSelectionModel =
    copy(
      requestAmountEditor = none,
      requestedAmounts = requestAmountEditor.foldLeft( requestedAmounts ):
        case ( amts, ( item, input ) ) =>
          amts.updatedWith( item )( _.map( v => input.input.flatMap( _.toDoubleOption ).getOrElse( v ) ) )
    )

  def requested: Vector[Countable[Double, ClassName[Item]]] =
    requestedAmounts
      .map:
        case ( item, amount ) => Countable( item, amount )
      .toVector

object RequestSelectionModel:
  val editorId: String = "request_amount_editor"

  def init: RequestSelectionModel = RequestSelectionModel( SortedMap.empty, SearchQuery.init, none )

  given Conversion[RequestSelectionModel, pp.RequestSelection]:
    override def apply( x: RequestSelectionModel ): pp.RequestSelection =
      pp.RequestSelection( x.requestedAmounts )

  def from( p: pp.RequestSelection ): RequestSelectionModel =
    RequestSelectionModel( p.requestedAmounts, SearchQuery.init, none )
