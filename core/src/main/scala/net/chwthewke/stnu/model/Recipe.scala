package net.chwthewke.stnu
package model

import cats.Order
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

  sealed trait NonExtraction extends Recipe:
    def category: RecipeCategory.Manufacturing | RecipeCategory.PowerGeneration

  case class Extraction(
      className: ClassName[Recipe.Extraction],
      displayName: String,
      category: RecipeCategory.Extraction,
      ingredients: List[Countable[Double, Item]],
      products: Countable[Double, Item],
      duration: FiniteDuration,
      producedIn: Machine,
      power: Power
  ) extends Recipe:
    type P[a] = a

    override def productsList: List[Countable[Double, Item]] = products :: Nil

  object Extraction:
    given Show[Recipe.Extraction]  = Show.show( showRecipe )
    given Order[Recipe.Extraction] = Order.by( _.displayName )

  case class Manufacturing(
      className: ClassName[Recipe.Manufacturing],
      displayName: String,
      category: RecipeCategory.Manufacturing,
      ingredients: List[Countable[Double, Item]],
      products: NonEmptyList[Countable[Double, Item]],
      duration: FiniteDuration,
      producedIn: Machine,
      power: Power
  ) extends Recipe
      with NonExtraction:
    type P[a] = NonEmptyList[a]

    override def productsList: List[Countable[Double, Item]] = products.toList

  object Manufacturing:
    given Show[Recipe.Manufacturing]  = Show.show( showRecipe )
    given Order[Recipe.Manufacturing] = Order.by( _.displayName )

  case class PowerGeneration(
      className: ClassName[Recipe.PowerGeneration],
      displayName: String,
      category: RecipeCategory.PowerGeneration,
      ingredients: List[Countable[Double, Item]],
      products: List[Countable[Double, Item]],
      duration: FiniteDuration,
      producedIn: Machine,
      power: Power
  ) extends Recipe
      with NonExtraction:
    type P[a] = List[a]

    override def productsList: List[Countable[Double, Item]] = products

  object PowerGeneration:
    given Show[Recipe.PowerGeneration]  = Show.show( showRecipe )
    given Order[Recipe.PowerGeneration] = Order.by( _.displayName )

  private def showRecipe( recipe: Recipe ): String =
    import recipe._
    show"""$displayName # $className (tier ${category.tier})
          |  Ingredients:
          |    ${ingredients.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
          |  Products:
          |    ${products.map( _.map( _.displayName ).show ).intercalate( "\n    " )}
          |  Duration: $duration
          |  Power: $power
          |  Produced in: ${producedIn.displayName}
          |""".stripMargin

  given Show[Recipe]  = Show.show( showRecipe )
  given Order[Recipe] = Order.by( _.displayName )
