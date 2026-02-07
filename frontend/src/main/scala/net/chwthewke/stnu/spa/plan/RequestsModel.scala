package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*
import scala.collection.immutable.SortedMap

import data.Countable
import model.Item

case class RequestsModel(
    requestAmountEditors: List[( Item, InputModel )],
    search: SearchQuery
):

  val requestedAmounts: SortedMap[ClassName[Item], Double] =
    requestAmountEditors
      .mapFilter:
        case ( item, input ) =>
          input.input.flatMap( _.toDoubleOption ).tupleLeft( item.className )
      .to( SortedMap )

  val requested: Vector[Countable[Double, ClassName[Item]]] =
    requestedAmounts
      .map:
        case ( item, amount ) => Countable( item, amount )
      .toVector

  val requestedItems: Set[ClassName[Item]] = requested.map( _.item ).toSet

  def restore: RequestsModel =
    RequestsModel(
      requestAmountEditors.map( _.map( _.restore ) ),
      search.restore
    )

  def update( msg: RequestsAction ): RequestsModel =
    msg match
      case RequestsAction.Delete( item ) =>
        copy( requestAmountEditors = requestAmountEditors.filterNot( _._1.className == item ) )
      case RequestsAction.SetAmountValue( actionItem, value ) =>
        copy( requestAmountEditors = requestAmountEditors.map:
          case ( item, inputModel ) =>
            ( item, if ( item.className == actionItem ) InputModel.onInput( value ) else inputModel ) )
      case RequestsAction.SearchInput( value ) =>
        copy( search = search.onInput( value ) )
      case RequestsAction.SearchReset =>
        copy( search = search.clear )
      case RequestsAction.RequestItem( item ) =>
        copy(requestAmountEditors =
          if ( requestAmountEditors.exists( _._1.className == item.className ) ) requestAmountEditors
          else requestAmountEditors :+ ( item, InputModel.withDefault( "1" ) )
        )

object RequestsModel:
  def apply( env: Env, requested: Iterable[( ClassName[Item], Double )] ): RequestsModel =
    RequestsModel(
      requested.toList.mapFilter:
        case ( itemClass, amount ) =>
          env.getItem( itemClass ).tupleRight( InputModel.withDefault( amount.toString ) ),
      SearchQuery.init
    )

  val init: RequestsModel = RequestsModel( Nil, SearchQuery.init )

  def editorId( item: ClassName[Item] ): String = show"request_editor_$item"

  given Conversion[RequestsModel, pp.RequestSelection]:
    override def apply( x: RequestsModel ): pp.RequestSelection =
      pp.RequestSelection( x.requestedAmounts )

  def from( env: Env, saved: pp.RequestSelection ): RequestsModel =
    RequestsModel( env, saved.requestedAmounts )
