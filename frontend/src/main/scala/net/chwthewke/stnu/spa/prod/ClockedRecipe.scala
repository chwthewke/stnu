package net.chwthewke.stnu
package spa.prod

import cats.data.NonEmptyList
import cats.syntax.all.*

import data.Countable
import model.Item
import model.Machine
import model.Power
import model.Recipe

/**
 * Represents a whole number of machines producing a recipe at a clock speed allowing for a given target production
 *
 *   - For extraction recipes, the number and clock speed are computed at once, going through the available resource
 *     nodes in order of decreasing purity
 *   - For manufacturing recipes, calculated from the required amount.
 *
 * @param recipes
 *   the fractional amount of machines producing the recipe
 * @param clockSpeed
 *   the clock speed of the machines
 * @param machineCount
 *   the integer amount of machines
 */
case class ClockedRecipe(
    recipes: Countable[Double, Recipe.Manufacturing],
    clockSpeed: ClockSpeed,
    machineCount: Int
) {

  def recipe: Recipe.Manufacturing = recipes.item

  def machine: Machine                 = recipes.item.producedIn
  def powerConsumptionExponent: Double = machine.powerConsumptionExponent

  val clockSpeedMillionth: Int = ( clockSpeed.toDouble * 10000d ).ceil.toInt

  def fractionalAmount: Double = recipes.amount

  def power: Power =
    recipes.item.power.map( _ * machineCount * math.pow( clockSpeedMillionth / 1e6d, powerConsumptionExponent ) )

  val mainProduct: Countable[Double, Item] = recipes.flatMap( _.productsPerMinute.head )
  val mainProductAmount: Double            = mainProduct.amount
  val mainProductAmountPerUnit: Double     = mainProductAmount / machineCount

  val ingredientsPerMinute: List[Countable[Double, Item]] = recipes.flatTraverse( _.ingredientsPerMinute )

  val productsPerMinute: NonEmptyList[Countable[Double, Item]] = recipes.flatTraverse( _.productsPerMinute )
}

object ClockedRecipe {
  def fixed( recipe: Recipe.Manufacturing, fractionalAmount: Double, amount: Int ): ClockedRecipe =
    ClockedRecipe( Countable( recipe, fractionalAmount ), ClockSpeed( fractionalAmount / amount * 100d ), amount )

  def roundUp( recipe: Countable[Double, Recipe.Manufacturing] ): ClockedRecipe =
    fixed( recipe.item, recipe.amount, recipe.amount.ceil.toInt )

  def overclocked( recipe: Countable[Int, Recipe.Manufacturing], clockSpeed: ClockSpeed ): ClockedRecipe =
    ClockedRecipe(
      Countable( recipe.item, recipe.amount.toDouble * clockSpeed.toDouble / 100d ),
      clockSpeed,
      recipe.amount
    )
}
