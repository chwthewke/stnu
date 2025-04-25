package net.chwthewke.stnu
package model

import cats.Foldable
import cats.Show
import cats.Traverse
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.concurrent.duration.*

import data.Countable

final case class Recipe[P[_]](
    className: ClassName,
    displayName: String,
    category: RecipeCategory,
    ingredients: List[Countable[Double, Item]],
    products: P[Countable[Double, Item]],
    duration: FiniteDuration,
    producedIn: Machine,
    power: Power
):
  def ingredientsPerMinute: List[Countable[Double, Item]]                = ingredients.map( perMinute )
  def productsPerMinute( using Traverse[P] ): P[Countable[Double, Item]] = products.map( perMinute )

  def itemsPerMinuteMap( using Traverse[P] ): Map[Item, Double] =
    productsPerMinute.foldMap:
      case Countable( it, am ) => Map( it -> am )
    |+|
      ingredientsPerMinute
        .foldMap:
          case Countable( it, am ) => Map( it -> -am )

  def itemsPerMinute( using Traverse[P] ): Vector[Countable[Double, Item]] =
    itemsPerMinuteMap
      .map:
        case ( item, amount ) => Countable( item, amount )
      .toVector

  def isExtraction: Boolean = producedIn.machineType.isExtractor

  private def perMinute( ct: Countable[Double, Item] ): Countable[Double, Item] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def isAlternate: Boolean = displayName.toLowerCase.startsWith( "alternate" )

  // NOTE iffy, but that's what we have
  def isMatterConversion( using Foldable[P] ): Boolean =
    producedIn.className == ClassName( "Build_Converter_C" ) &&
      ingredients.size == 2 &&
      ingredients.exists( _.item.className == ClassName( "Desc_SAMIngot_C" ) ) &&
      products.size == 1 &&
      products.toIterable.headOption.forall( _.item.className != ClassName( "Desc_FicsiteIngot_C" ) )

object Recipe:

  type Prod = Recipe[NonEmptyList]
  object Prod:
    def apply(
        className: ClassName,
        displayName: String,
        category: RecipeCategory,
        ingredients: List[Countable[Double, Item]],
        products: NonEmptyList[Countable[Double, Item]],
        duration: FiniteDuration,
        producedIn: Machine,
        power: Power
    ): Recipe.Prod =
      Recipe( className, displayName, category, ingredients, products, duration, producedIn, power )

  type PowerGen = Recipe[List]
  object PowerGen:
    def apply(
        className: ClassName,
        displayName: String,
        category: RecipeCategory,
        ingredients: List[Countable[Double, Item]],
        products: List[Countable[Double, Item]],
        duration: FiniteDuration,
        producedIn: Machine,
        power: Power
    ): Recipe.PowerGen =
      Recipe( className, displayName, category, ingredients, products, duration, producedIn, power )

  given [O[_]: Traverse] => Show[Recipe[O]] =
    Show.show:
      case Recipe( className, displayName, category, ingredients, products, duration, producer, power ) =>
        show"""  $displayName # $className ${category.tierOpt.map( t => s"(tier $t)" ).orEmpty}
              |  Ingredients:
              |    ${ingredients.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
              |  Products:
              |    ${products.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
              |  Duration: $duration
              |  Power: $power
              |  Produced in: ${producer.displayName}
              |""".stripMargin
