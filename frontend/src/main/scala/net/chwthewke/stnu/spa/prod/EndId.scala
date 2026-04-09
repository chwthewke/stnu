package net.chwthewke.stnu
package spa
package prod

import cats.Order
import cats.Show

import data.Countable
import model.Item
import model.Recipe

enum EndId:
  case Process( recipe: ClassName[Recipe], boost: Int )
  case Input( item: Countable[Double, ClassName[Item]] )
  case Requested( item: Countable[Double, ClassName[Item]] )
  case Byproduct( item: Countable[Double, ClassName[Item]] )

object EndId:
  def process( process: ClockedRecipe ): Process =
    Process( process.recipe.className, process.boostedRecipe.usedSlots )

  private def toOrdered( endId: EndId ): ( Int, String, Double ) =
    endId match
      case EndId.Process( recipe, boost ) => ( 0, recipe.name, boost.toDouble )
      case EndId.Input( item )            => ( 1, item.item.name, item.amount )
      case EndId.Requested( item )        => ( 2, item.item.name, item.amount )
      case EndId.Byproduct( item )        => ( 3, item.item.name, item.amount )

  given Show[EndId]     = Show.fromToString[EndId]
  given Order[EndId]    = Order.by( toOrdered )
  given Ordering[EndId] = Order.catsKernelOrderingForOrder
