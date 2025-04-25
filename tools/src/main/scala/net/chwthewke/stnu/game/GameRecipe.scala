package net.chwthewke.stnu
package game

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Decoder
import scala.concurrent.duration.*

import data.Countable

case class GameRecipe(
    className: ClassName,
    displayName: String,
    ingredients: List[Countable[Double, ClassName]],
    products: NonEmptyList[Countable[Double, ClassName]],
    duration: FiniteDuration,
    producedIn: List[ClassName],
    variablePowerMin: Double,
    variablePowerRange: Double
):
  def isSelfExtraction: Boolean = ingredients == List( products.head )

object GameRecipe:

  given Decoder[GameRecipe] =

    import Parsers.*

    Decoder.forProduct8(
      "ClassName",
      "mDisplayName",
      "mIngredients",
      "mProduct",
      "mManufactoringDuration",
      "mProducedIn",
      "mVariablePowerConsumptionConstant",
      "mVariablePowerConsumptionFactor"
    )(
      (
          cn: ClassName,
          dn: String,
          in: List[Countable[Double, ClassName]],
          out: NonEmptyList[Countable[Double, ClassName]],
          dur: FiniteDuration,
          mch: List[ClassName],
          pmin: Double,
          prg: Double
      ) => GameRecipe( cn, dn, in, out, dur, mch, pmin, prg )
    )(
      Decoder[ClassName],
      Decoder[String],
      countableListOrEmpty.decoder,
      countableList.decoder,
      Decoders.doubleStringDecoder.map( _.seconds ),
      Decoder.decodeOption( manufacturerClassList.decoder ).map( _.orEmpty ),
      Decoders.doubleStringDecoder,
      Decoders.doubleStringDecoder
    )

  given Show[GameRecipe] =
    Show.show:
      case GameRecipe( className, displayName, ingredients, products, duration, producers, powerMin, powerRange ) =>
        show"""  $displayName # $className
              |  Ingredients:
              |    ${ingredients.map( _.show ).intercalate( "\n    " )}
              |  Products:
              |    ${products.map( _.show ).intercalate( "\n    " )}
              |  Duration $duration
              |  Producers: ${producers.mkString_( ", " )}
              |  Power: ${f"$powerMin%.3f-$powerRange%.3f"}
              |""".stripMargin
