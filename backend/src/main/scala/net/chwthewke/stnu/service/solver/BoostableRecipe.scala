package net.chwthewke.stnu
package service
package solver

import cats.syntax.all.*

import data.Countable
import model.ClockSpeed
import model.Form
import model.Machine
import model.Recipe
import model.Transport

case class BoostableRecipe(
    recipe: Recipe.NonExtraction,
    maxClockSpeed: ClockSpeed,
    maxBoost: Int
)

object BoostableRecipe:
  def apply( recipe: Recipe.NonExtraction, bestConveyorBelt: Transport, bestPipeline: Transport ): BoostableRecipe =
    // the maximum clock speed (without boost) to be able to input ingredients & output products with the
    //  selected transport
    val clockSpeed: ClockSpeed =
      recipe.itemsPerMinuteMap
        .map:
          case ( item, amount ) =>
            val maxAmount: Double =
              ( if ( item.form == Form.Solid ) bestConveyorBelt else bestPipeline ).perMinute.toDouble
            val frac = maxAmount / amount.abs
            ClockSpeed.ofFraction( frac )
        .toVector
        .min // unsafe but no recipe has neither ingredient nor product

    //
    val maxBoost: Int =
      recipe.productionBoost
        .flatMap: boost =>
          boost.slots
            .to( 1, step = -1 )
            .find: b =>
              val bufferStacks: Int = if ( recipe.producedIn.className == Machine.quantumEncoder ) 2 else 1
              recipe.productsList.forall:
                case Countable( item, amount ) =>
                  2 * amount * ( 1 + b * boost.effect ) <= item.stackSize * bufferStacks
        .orEmpty

    BoostableRecipe( recipe, clockSpeed, maxBoost )
