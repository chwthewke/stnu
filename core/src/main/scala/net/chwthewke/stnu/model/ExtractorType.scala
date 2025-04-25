package net.chwthewke.stnu
package model

enum ExtractorType(
    val description: String,
    val dataKey: Either[String, ClassName]
):

  case Miner     extends ExtractorType( "miner", Left( "Miner" ) )
  case WaterPump extends ExtractorType( "water extractor", Right( ExtractorType.waterExtractorClass ) )
  case OilPump   extends ExtractorType( "oil extractor", Right( ExtractorType.oilExtractorClass ) )
  case Fracking  extends ExtractorType( "fracking extractor", Right( ExtractorType.frackingExtractorClass ) )

object ExtractorType
    extends Enum[ExtractorType]
    with CatsEnum[ExtractorType]
    with OrderEnum[ExtractorType]
    with CirceEnum[ExtractorType]:
  private def waterExtractorClass: ClassName    = ClassName( "Build_WaterPump_C" )
  private def oilExtractorClass: ClassName      = ClassName( "Build_OilPump_C" )
  private def frackingExtractorClass: ClassName = ClassName( "Build_FrackingExtractor_C" )
