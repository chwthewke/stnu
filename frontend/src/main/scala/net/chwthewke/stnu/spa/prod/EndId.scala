package net.chwthewke.stnu
package spa
package prod

import cats.Order
import cats.Show

import data.Countable
import model.Item
import model.Recipe

enum EndId:
  case Process( recipe: ClassName[Recipe] )
  case Input( item: Countable[Double, ClassName[Item]] )
  case Requested( item: Countable[Double, ClassName[Item]] )
  case Byproduct( item: Countable[Double, ClassName[Item]] )

object EndId:
  private def toOrdered( endId: EndId ): ( Int, String, Double ) =
    endId match
      case EndId.Process( recipe ) => ( 0, recipe.name, 0d )
      case EndId.Input( item )     => ( 1, item.item.name, item.amount )
      case EndId.Requested( item ) => ( 2, item.item.name, item.amount )
      case EndId.Byproduct( item ) => ( 3, item.item.name, item.amount )

  given Show[EndId]     = Show.fromToString[EndId]
  given Order[EndId]    = Order.by( toOrdered )
  given Ordering[EndId] = Order.catsKernelOrderingForOrder
