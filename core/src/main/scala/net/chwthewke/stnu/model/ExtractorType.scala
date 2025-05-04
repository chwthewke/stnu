package net.chwthewke.stnu
package model

enum ExtractorType(
    val description: String,
    val dataKey: Either[String, ClassName[Machine]]
):

  case Miner       extends ExtractorType( "miner", Left( "Miner" ) )
  case WaterPump   extends ExtractorType( "water extractor", Right( ExtractorType.waterExtractorClass ) )
  case OilPump     extends ExtractorType( "oil extractor", Right( ExtractorType.oilExtractorClass ) )
  case Fracking    extends ExtractorType( "fracking extractor", Right( ExtractorType.frackingExtractorClass ) )
  case FicsmasTree extends ExtractorType( "FICSMAS tree", Right( ExtractorType.ficsmasTree ) )

object ExtractorType
    extends Enum[ExtractorType]
    with CatsEnum[ExtractorType]
    with OrderEnum[ExtractorType]
    with CirceEnum[ExtractorType]:
  private def waterExtractorClass: ClassName[Machine]    = ClassName( "Build_WaterPump_C" )
  private def oilExtractorClass: ClassName[Machine]      = ClassName( "Build_OilPump_C" )
  private def frackingExtractorClass: ClassName[Machine] = ClassName( "Build_FrackingExtractor_C" )
  private def ficsmasTree: ClassName[Machine]            = ClassName( "Build_TreeGiftProducer_C" )
