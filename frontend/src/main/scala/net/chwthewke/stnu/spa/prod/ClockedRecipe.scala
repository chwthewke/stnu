package net.chwthewke.stnu
package spa.prod

import cats.syntax.all.*

import data.Countable
import model.ClockSpeed
import model.ClockSpeedPreset
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
 * @param clockSpeedPreset
 *   the max clock speed
 * @param machineCount
 *   the integer amount of machines
 */
case class ClockedRecipe(
    recipes: Countable[Double, Recipe],
    clockSpeed: ClockSpeed,
    clockSpeedPreset: ClockSpeedPreset,
    machineCount: Int
) {

  def recipe: Recipe = recipes.item

  def machine: Machine                 = recipes.item.producedIn
  def powerConsumptionExponent: Double = machine.powerConsumptionExponent

  val clockSpeedMillionth: Int = ( clockSpeed.percent * 10000d ).ceil.toInt

  def fractionalAmount: Double = recipes.amount

  def power: Power =
    recipes.item.power.map(
      consumed = _ * machineCount * math.pow( clockSpeedMillionth / 1e6d, powerConsumptionExponent ),
      produced = _ * machineCount * clockSpeedMillionth / 1e6d
    )

  val mainProduct: Option[Countable[Double, Item]] = recipes.flatTraverse( _.productsPerMinute.toList.headOption )
  val mainProductAmount: Double                    = mainProduct.foldMap( _.amount )
  val mainProductAmountPerUnit: Double             = mainProductAmount / machineCount

  val ingredientsPerMinute: List[Countable[Double, Item]] = recipes.flatTraverse( _.ingredientsPerMinute )

  val productsPerMinute: List[Countable[Double, Item]] = recipes.flatTraverse( _.productsPerMinute.toList )

  def itemsPerMinute: List[Countable[Double, Item]] =
    ( ingredientsPerMinute.map( _.mapAmount( am => -am ) ) ++ productsPerMinute ).gather
      .mapFilter( _.significant )

  def mapAmount( f: Double => Double ): ClockedRecipe =
    recipe match
      case x: Recipe.NonExtraction => ClockedRecipe.roundUp( Countable( x, f( fractionalAmount ) ) )
      case r: Recipe.Extraction => ClockedRecipe.overclocked( Countable( r, f( fractionalAmount ) ), clockSpeedPreset )

  def times( d: Double ): ClockedRecipe = mapAmount( _ * d )
}

object ClockedRecipe {
  def fixed( recipe: Recipe, fractionalAmount: Double, preset: ClockSpeedPreset, amount: Int ): ClockedRecipe =
    ClockedRecipe(
      Countable( recipe, fractionalAmount ),
      ClockSpeed.ofFraction( fractionalAmount / amount ),
      preset,
      amount
    )

  def roundUp( recipe: Countable[Double, Recipe.NonExtraction] ): ClockedRecipe =
    fixed( recipe.item, recipe.amount, ClockSpeedPreset.`100%`, recipe.amount.ceil.toInt )

  def overclocked( recipe: Countable[Double, Recipe.Extraction], clockSpeedLimit: ClockSpeedPreset ): ClockedRecipe =
    val intAmount: Int = math.ceil( recipe.amount / clockSpeedLimit.value.fraction ).toInt
    ClockedRecipe.fixed( recipe.item, recipe.amount, clockSpeedLimit, intAmount )

}
