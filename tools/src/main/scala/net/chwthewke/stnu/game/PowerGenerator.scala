package net.chwthewke.stnu
package game

import cats.Show
import cats.syntax.all.*
import io.circe.Decoder

import data.Countable

case class PowerGenerator(
    className: ClassName,
    displayName: String,
    powerProduction: Double, // MW
    powerConsumptionExponent: Double,
    supplementalToPowerRatio: Double, // L/MJ
    fuels: Vector[PowerGenerator.GeneratorFuel]
)

object PowerGenerator:

  given Show[PowerGenerator] = Show: generator =>
    show"""${generator.displayName} # ${generator.className}
          |Power: ${f"${generator.powerProduction}%.0f"} MW (exp: ${f"${generator.powerConsumptionExponent}%.4f"})
          |Fuels:
          |  ${generator.fuels.sortBy( _.fuel ).mkString_( "\n  " )}
          |Suppl. resource to power ratio: ${f"${generator.supplementalToPowerRatio}%.4f"}
          |""".stripMargin

  case class GeneratorFuel(
      fuel: ClassName,
      byproduct: Option[Countable[Int, ClassName]],
      supplementalResource: Option[ClassName]
  )

  object GeneratorFuel:
    given Show[GeneratorFuel] = Show.show: fuel =>
      val showSupplemental = fuel.supplementalResource.map( cn => show" (with $cn)" ).orEmpty
      val showByproduct    = fuel.byproduct.map( cn => show" -> $cn" ).orEmpty
      show"${fuel.fuel}$showSupplemental$showByproduct"

  private object Types:
    opaque type ByproductAmount = Int
    object ByproductAmount:
      inline def apply( amount: Int ): ByproductAmount    = amount
      extension ( self: ByproductAmount ) def amount: Int = self
      given Decoder[ByproductAmount] =
        Decoder
          .instance( hc =>
            hc.as[String] match
              case Right( "" ) => Right( 0 )
              case _           => hc.as[Int]
          )

  given Decoder[Vector[GeneratorFuel]] =
    given Decoder[Option[ClassName]] =
      Decoder.decodeString.map( str => Option.when( str.nonEmpty )( ClassName( str ) ) )

    given Decoder[GeneratorFuel] =
      Decoder.forProduct4[GeneratorFuel, ClassName, Option[ClassName], Types.ByproductAmount, Option[ClassName]](
        "mFuelClass",
        "mByproduct",
        "mByproductAmount",
        "mSupplementalResourceClass"
      )( ( f, p, a, s ) => GeneratorFuel( f, p.map( Countable( _, a.amount ) ), s ) )

    Decoder.decodeVector[GeneratorFuel]

  given Decoder[PowerGenerator] =
    Decoder.forProduct6[PowerGenerator, ClassName, String, Double, Double, Double, Vector[GeneratorFuel]](
      "ClassName",
      "mDisplayName",
      "mPowerProduction",
      "mPowerConsumptionExponent",
      "mSupplementalToPowerRatio",
      "mFuel"
    )( PowerGenerator( _, _, _, _, _, _ ) )
