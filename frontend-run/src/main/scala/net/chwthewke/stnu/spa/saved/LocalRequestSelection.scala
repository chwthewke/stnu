package net.chwthewke.stnu
package spa
package saved

import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder
import scala.collection.immutable.SortedMap

import model.Item
import spa.plan.RequestSelectionModel

object LocalRequestSelection:
  case class Saved(
      items: SortedMap[ClassName[Item], Double]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( requestSelection: RequestSelectionModel ): Saved =
      Saved(
        requestSelection.requestedAmounts.map:
          case ( item, amt ) => ( item.className, amt )
      )

  case class Loaded(
      items: SortedMap[ClassName[Item], Double]
  ) derives ConfiguredDecoder:
    def toRequestSelection( env: Env ): RequestSelectionModel =
      RequestSelectionModel(
        items.flatMap:
          case ( cn, amt ) => env.game.items.get( cn ).tupleRight( amt ),
        SearchQuery.init,
        none
      )
