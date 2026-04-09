package net.chwthewke.stnu
package spa.prod

import cats.syntax.all.*

import data.Countable
import model.ClockSpeed
import model.Item
import model.Machine
import model.Power
import model.Recipe
import protocol.solver.BoostedRecipe

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
    recipes: Countable[Double, BoostedRecipe[Recipe]],
    clockSpeed: ClockSpeed,
    machineCount: Int
) {

  val boostedRecipe: BoostedRecipe[Recipe] = recipes.item
  def recipe: Recipe                       = boostedRecipe.recipe

  def machine: Machine                 = recipe.producedIn
  def powerConsumptionExponent: Double = machine.powerConsumptionExponent

  val clockSpeedMillionth: Int = ( clockSpeed.percent * 10000d ).ceil.toInt

  def fractionalAmount: Double = recipes.amount

  def power: Power =
    boostedRecipe.power.map(
      consumed = _ * machineCount * math.pow( clockSpeedMillionth / 1e6d, powerConsumptionExponent ),
      produced = _ * machineCount * clockSpeedMillionth / 1e6d
    )

  val mainProduct: Option[Countable[Double, Item]] = recipes.flatTraverse( _.productsPerMinute.toList.headOption )
  val mainProductAmount: Double                    = mainProduct.foldMap( _.amount )
  val mainProductAmountPerUnit: Double             = mainProductAmount / machineCount

  val ingredientsPerMinute: List[Countable[Double, Item]] = recipes.flatTraverse( _.ingredientsPerMinute )

  val productsPerMinute: List[Countable[Double, Item]] = recipes.flatTraverse( _.productsPerMinute.toList )

  def productionBoostShards: Int = machineCount * boostedRecipe.usedSlots

  def powerShards: Int = machineCount * math.ceil( 2d * ( clockSpeed.fraction - 1d ).max( 0d ) ).toInt

  def itemsPerMinute: List[Countable[Double, Item]] =
    ( ingredientsPerMinute.map( _.mapAmount( am => -am ) ) ++ productsPerMinute ).gather
      .mapFilter( _.significant )

  def mapAmount( f: Double => Double ): ClockedRecipe =
    ClockedRecipe.overclocked( Countable( boostedRecipe, f( fractionalAmount ) ) )

  def times( d: Double ): ClockedRecipe = mapAmount( _ * d )
}

object ClockedRecipe {
  def fixed( recipe: BoostedRecipe[Recipe], fractionalAmount: Double, amount: Int ): ClockedRecipe =
    ClockedRecipe(
      Countable( recipe, fractionalAmount ),
      ClockSpeed.ofFraction( fractionalAmount / amount ),
      amount
    )

  // NOTE corrects amount that very slightly exceed the capacity of a machine as precision errors
  private def amountCorrection( amount: Double ): Double =
    val floor: Double = amount.floor
    if ( amount - floor < Countable.Tolerance ) floor else amount

  def overclocked( recipe: Countable[Double, BoostedRecipe[Recipe]] ): ClockedRecipe =
    val fractionalMachineCount: Double = amountCorrection( recipe.amount / recipe.item.maxClockSpeed.fraction )
    val machineCount: Int              = math.ceil( fractionalMachineCount ).toInt
    val recipeCount: Double            = fractionalMachineCount * recipe.item.maxClockSpeed.fraction
    ClockedRecipe(
      Countable( recipe.item, recipeCount ),
      ClockSpeed.ofFraction( recipeCount / machineCount ),
      machineCount
    )

}
