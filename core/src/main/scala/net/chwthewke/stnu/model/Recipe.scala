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

  given Traverse[P] = compiletime.deferred

  def className: ClassName[Recipe]
  def displayName: String
  def category: RecipeCategory
  def ingredients: List[Countable[Double, Item]]
  def products: P[Countable[Double, Item]]
  final def productsList: List[Countable[Double, Item]] = products.toList
  def duration: FiniteDuration
  def producedIn: Machine
  def power: Power

  def ingredientsPerMinute: List[Countable[Double, Item]] = ingredients.map( perMinute )
  def productsPerMinute: P[Countable[Double, Item]]       = products.map( perMinute )
  def items: List[Item] = ( ingredients.map( _.item ) ++ productsList.map( _.item ) ).distinctBy( _.className )

  def itemsPerMinuteMap: Map[Item, Double] =
    productsPerMinute.foldMap:
      case Countable( it, am ) => Map( it -> am )
    |+|
      ingredientsPerMinute
        .foldMap:
          case Countable( it, am ) => Map( it -> -am )

  private def perMinute( ct: Countable[Double, Item] ): Countable[Double, Item] =
    Countable( ct.item, ct.amount * 60000 / duration.toMillis )

  def isAlternate: Boolean     = displayName.toLowerCase.startsWith( "alternate" )
  def displayNameNoAlt: String = displayName.stripPrefix( "Alternate: " )

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
    override def className: ClassName[Recipe.NonExtraction]

  object NonExtraction:
    given Show[Recipe.NonExtraction]     = Show.show( showRecipe )
    given Order[Recipe.NonExtraction]    = Order.by( _.displayNameNoAlt )
    given Ordering[Recipe.NonExtraction] = Order.catsKernelOrderingForOrder

  case class Extraction(
      className: ClassName[Recipe.Extraction],
      displayName: String,
      category: RecipeCategory.Extraction,
      ingredients: List[Countable[Double, Item]],
      products: Countable[Double, Item],
      duration: FiniteDuration,
      producedIn: Machine,
      purity: Option[ResourcePurity],
      power: Power
  ) extends Recipe:
    type P[a] = a

  object Extraction:
    private[Recipe] def orderKey( r: Extraction ): ( String, String, String ) =
      ( r.displayNameNoAlt, r.producedIn.displayName, r.purity.foldMap( _.show ) )

    given Show[Recipe.Extraction]     = Show.show( showRecipe )
    given Order[Recipe.Extraction]    = Order.by( orderKey )
    given Ordering[Recipe.Extraction] = Order.catsKernelOrderingForOrder

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

  object Manufacturing:
    given Show[Recipe.Manufacturing]     = Show.show( showRecipe )
    given Order[Recipe.Manufacturing]    = Order.by( _.displayNameNoAlt )
    given Ordering[Recipe.Manufacturing] = Order.catsKernelOrderingForOrder

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

  object PowerGeneration:
    given Show[Recipe.PowerGeneration]     = Show.show( showRecipe )
    given Order[Recipe.PowerGeneration]    = Order.by( _.displayNameNoAlt )
    given Ordering[Recipe.PowerGeneration] = Order.catsKernelOrderingForOrder

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

  private def orderKey( recipe: Recipe ): ( String, String, String ) =
    recipe match
      case e: Extraction => Extraction.orderKey( e )
      case _             => ( recipe.displayName, "", "" )

  given Show[Recipe]     = Show.show( showRecipe )
  given Order[Recipe]    = Order.by( orderKey )
  given Ordering[Recipe] = Order.catsKernelOrderingForOrder
