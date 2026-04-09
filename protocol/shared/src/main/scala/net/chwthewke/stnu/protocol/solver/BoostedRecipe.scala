package net.chwthewke.stnu
package protocol
package solver

import cats.Show
import cats.Traverse
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder

import data.Countable
import model.ClockSpeed
import model.ClockSpeedPreset
import model.Item
import model.Power
import model.ProductionBoost
import model.Recipe

case class BoostedRecipe[+R]( recipe: R, usedSlots: Int, maxClockSpeed: ClockSpeed ) derives Show, Traverse

object BoostedRecipe:
  private val recipeField        = "recipe"
  private val usedSlotsField     = "usedSlots"
  private val maxClockSpeedField = "maxClockSpeed"

  given [R: Decoder] => Decoder[BoostedRecipe[R]] =
    Decoder.forProduct3( recipeField, usedSlotsField, maxClockSpeedField )( BoostedRecipe[R] )
  given [R: Encoder] => Encoder[BoostedRecipe[R]] =
    Encoder.forProduct3( recipeField, usedSlotsField, maxClockSpeedField )( br =>
      ( br.recipe, br.usedSlots, br.maxClockSpeed )
    )

  def apply[R]( recipe: R, maxClockSpeed: ClockSpeedPreset ): BoostedRecipe[R] =
    BoostedRecipe( recipe, 0, maxClockSpeed.value )

  extension [R <: Recipe]( self: BoostedRecipe[R] )
    def productionBoost: Option[ProductionBoost] = self.recipe.productionBoost

    def power: Power =
      self.recipe.power
        .map(
          c => c * productionBoost.powerConsumption( self.usedSlots ),
          identity
        )

    def ingredientsPerMinute: List[Countable[Double, Item]] = self.recipe.ingredientsPerMinute

    def productsPerMinute: self.recipe.P[Countable[Double, Item]] =
      self.recipe.productsPerMinute.map( _.mapAmount( _ * ( 1d + productionBoost.effect( self.usedSlots ) ) ) )

    def itemsPerMinuteMap( clockSpeed: ClockSpeed ): Map[Item, Double] =
      ( productsPerMinute.foldMap { case Countable( it, am ) => Map( it -> am ) }
        |+|
          ingredientsPerMinute.foldMap { case Countable( it, am ) => Map( it -> -am ) } )
        .fmap( _ * clockSpeed.fraction )
