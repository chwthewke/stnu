package net.chwthewke.stnu
package spa
package plan

import model.ExtractorType
import model.Item
import model.Machine
import model.Model
import spa.prod.ClockSpeed

case class ExtractionOptionsInputModel(
    minerClass: ClassName[Machine],
    clockSpeed: ClockSpeed,
    extractors: Set[ExtractorType],
    preferFracking: Set[ClassName[Item]],
    resourceWeights: Map[ClassName[Item], Int]
):
  def setOption( extractionOption: ExtractionOption ): ExtractionOptionsInputModel = extractionOption match
    case ExtractionOption.SetMiner( machine )         => copy( minerClass = machine )
    case ExtractionOption.SetClockSpeed( clockSpeed ) => copy( clockSpeed = clockSpeed )
    case ExtractionOption.ToggleExtractorType( extractor, value ) =>
      copy( extractors = toggle( extractors, extractor, value ) )
    case ExtractionOption.ToggleFrackingPreference( item, value ) =>
      copy( preferFracking = toggle( preferFracking, item, value ) )
    case ExtractionOption.SetResourceWeight( item, value ) =>
      value.toIntOption.fold( this ): w =>
        copy( resourceWeights = resourceWeights.updated( item, w ) )

  private def toggle[A]( set: Set[A], key: A, previousValue: String ): Set[A] =
    val wasEnabled: Boolean = previousValue.toBooleanOption.getOrElse( false )
    if ( wasEnabled ) set - key else set + key

object ExtractionOptionsInputModel:
  def init( game: Model ): ExtractionOptionsInputModel =
    ExtractionOptionsInputModel(
      game.machines.values
        .filter( _.machineType.extractor.contains( ExtractorType.Miner ) )
        .maxBy( _.powerConsumption )
        .className,
      ClockSpeed.`100%`,
      ExtractorType.cases.toSet,
      Set.empty,
      game.extractedItems.map( item => ( item.className, 4 ) ).toMap
    )
