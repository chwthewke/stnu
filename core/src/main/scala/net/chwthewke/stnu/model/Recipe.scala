package net.chwthewke.stnu
package model

import cats.Show
import cats.Traverse
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.concurrent.duration.*

import data.Countable

sealed trait Recipe:
  type P[a]

  protected given Traverse[P] = compiletime.deferred

  def className: ClassName[Recipe]
  def displayName: String
  def category: RecipeCategory
  def ingredients: List[Countable[Double, Item]]
  def products: P[Countable[Double, Item]]
  def productsList: List[Countable[Double, Item]]
  def duration: FiniteDuration
  def producedIn: Machine
  def power: Power

  def ingredientsPerMinute: List[Countable[Double, Item]] = ingredients.map( perMinute )
  def productsPerMinute: P[Countable[Double, Item]]       = products.map( perMinute )

  def itemsPerMinuteMap: Map[Item, Double] =
    productsPerMinute.foldMap:
      case Countable( it, am ) => Map( it -> am )
    |+|
      ingredientsPerMinute
        .foldMap:
          case Countable( it, am ) => Map( it -> -am )

  def itemsPerMinute: Vector[Countable[Double, Item]] =
    itemsPerMinuteMap
      .map:
        case ( item, amount ) => Countable( item, amount )
      .toVector

  def isExtraction: Boolean = producedIn.machineType.isExtractor

  private def perMinute( ct: Countable[Double, Item] ): Countable[Double, Item] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def isAlternate: Boolean = displayName.toLowerCase.startsWith( "alternate" )

  // NOTE iffy, but that's what we have
  def isMatterConversion: Boolean =
    producedIn.className == ClassName( "Build_Converter_C" ) &&
      ingredients.size == 2 &&
      ingredients.exists( _.item.className == ClassName( "Desc_SAMIngot_C" ) ) &&
      products.size == 1 &&
      products.toIterable.headOption.forall( _.item.className != ClassName( "Desc_FicsiteIngot_C" ) )

object Recipe:

  case class Prod(
      className: ClassName[Recipe.Prod],
      displayName: String,
      category: RecipeCategory,
      ingredients: List[Countable[Double, Item]],
      products: NonEmptyList[Countable[Double, Item]],
      duration: FiniteDuration,
      producedIn: Machine,
      power: Power
  ) extends Recipe:
    type P[a] = NonEmptyList[a]

    override def productsList: List[Countable[Double, Item]] = products.toList

  object Prod:
    given Show[Recipe.Prod] = Show.show( showRecipe )

  case class PowerGen(
      className: ClassName[Recipe.PowerGen],
      displayName: String,
      category: RecipeCategory,
      ingredients: List[Countable[Double, Item]],
      products: List[Countable[Double, Item]],
      duration: FiniteDuration,
      producedIn: Machine,
      power: Power
  ) extends Recipe:
    type P[a] = List[a]

    override def productsList: List[Countable[Double, Item]] = products

  object PowerGen:
    given Show[Recipe.PowerGen] = Show.show( showRecipe )

  private def showRecipe( recipe: Recipe ): String =
    import recipe._
    show"""$displayName # $className ${category.tierOpt.map( t => s"(tier $t)" ).orEmpty}
          |  Ingredients:
          |    ${ingredients.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
          |  Products:
          |    ${products.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
          |  Duration: $duration
          |  Power: $power
          |  Produced in: ${producedIn.displayName}
          |""".stripMargin

  given Show[Recipe] = Show.show( showRecipe )
