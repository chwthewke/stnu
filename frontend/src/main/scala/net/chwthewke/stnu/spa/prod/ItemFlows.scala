package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyVector
import cats.syntax.all.*
import scala.collection.immutable.SortedMap

import model.Item
import model.Recipe
import model.prod.FlowEnd
import protocol.persistence.ProcessSplitId

case class ItemFlows(
    transports: NonEmptyVector[ItemTransportRef],
    transportSplits: Vector[TransportSplit]
)

object ItemFlows:
  def init(
      productionRows: List[ClockedRecipe],
      endProcessSplitIds: SortedMap[EndId, ProcessSplitId]
  ): Map[ClassName[Item], ItemFlows] =
    val processesByClass: Map[ClassName[Recipe], ClockedRecipe] =
      productionRows.fproductLeft( _.recipe.className ).toMap

    endProcessSplitIds.toVector
      .foldMap:
        case ( EndId.Process( recipeClass, _ ), id ) =>
          processesByClass
            .get( recipeClass )
            .foldMap: cr =>
              cr.ingredientsPerMinute
                .foldMap( ci => Map( ci.item.className -> Map( FlowEnd.destination -> NonEmptyVector.one( id ) ) ) )
                |+| cr.productsPerMinute
                  .foldMap( ci => Map( ci.item.className -> Map( FlowEnd.source -> NonEmptyVector.one( id ) ) ) )
        case ( EndId.Input( ci ), id ) =>
          Map( ci.item -> Map( FlowEnd.source -> NonEmptyVector.one( id ) ) )
        case ( EndId.Requested( ci ), id ) =>
          Map( ci.item -> Map( FlowEnd.destination -> NonEmptyVector.one( id ) ) )
        case ( EndId.Byproduct( ci ), id ) =>
          Map( ci.item -> Map( FlowEnd.destination -> NonEmptyVector.one( id ) ) )
      .fmap( byEnd => ItemFlows( NonEmptyVector.one( ItemTransportRef( byEnd ) ), Vector.empty ) )
