package net.chwthewke.stnu
package game

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Decoder
import mouse.boolean.*
import mouse.option.*
import scala.concurrent.duration.*

final case class Extractor(
    className: ClassName,
    displayName: String,
    extractorTypeName: String,
    allowedResourceForms: List[GameForm],
    allowedResources: Option[NonEmptyList[ClassName]],
    powerConsumption: Double,
    powerConsumptionExponent: Double,
    cycleTime: FiniteDuration,
    itemsPerCycle: Int
)

object Extractor:

  val converterClass: ClassName = ClassName( "Build_Converter_C" )

  val waterExtractorClass: ClassName    = ClassName( "Build_WaterPump_C" )
  val oilExtractorClass: ClassName      = ClassName( "Build_OilPump_C" )
  val frackingExtractorClass: ClassName = ClassName( "Build_FrackingExtractor_C" )

  private def of(
      cn: ClassName,
      dn: String,
      etn: String,
      arf: List[GameForm],
      fr: Boolean,
      rf: List[ClassName],
      pc: Double,
      pe: Double,
      ct: Double,
      ic: Int
  ): Extractor =
    Extractor( cn, dn, etn, arf, NonEmptyList.fromList( rf ).flatMap( fr.option( _ ) ), pc, pe, ct.seconds, ic )

  given Decoder[Extractor] =
    import Parsers.*

    given Decoder[Boolean]             = Decoders.booleanStringDecoder
    given Decoder[Double]              = Decoders.doubleStringDecoder
    given Decoder[Int]                 = Decoders.intStringDecoder
    given dlf: Decoder[List[GameForm]] = listOf( GameForm.parser ).decoder
    given dlc: Decoder[List[ClassName]] = listOf( bpGeneratedClass ).decoder
      .or( Decoder[String].ensure( _.isEmpty, "Cannot decode allowed resources" ).as( List.empty[ClassName] ) )

    Decoder.forProduct10(
      "ClassName",
      "mDisplayName",
      "mExtractorTypeName",
      "mAllowedResourceForms",
      "mOnlyAllowCertainResources",
      "mAllowedResources",
      "mPowerConsumption",
      "mPowerConsumptionExponent",
      "mExtractCycleTime",
      "mItemsPerCycle"
    )( Extractor.of )

  given Show[Extractor] = Show: extractor =>
    show"""${extractor.displayName} # ${extractor.className}
          |${extractor.itemsPerCycle} / ${extractor.cycleTime}
          |Power: ${f"${extractor.powerConsumption}%.0f"} MW (exp: ${f"${extractor.powerConsumptionExponent}%.4f"})
          |Resource forms: ${extractor.allowedResourceForms.map( _.show ).intercalate( ", " )}
          |Resources: ${extractor.allowedResources.cata( _.toList.map( _.show ).intercalate( ", " ), "any" )}
          |""".stripMargin
