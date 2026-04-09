package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine
import model.Model
import model.ResourceDistrib
import model.ResourceWeights
import protocol.solver.SolverRequest

case class ExtractionOptions(
    minerClass: ClassName[Machine],
    clockSpeed: ClockSpeedPreset.Extraction,
    excludeWaterPumpFromOverclocking: Boolean,
    extractors: Set[ExtractorType],
    preferFracking: Set[ClassName[Item]],
    resourceWeightSliders: Map[ClassName[Item], Int]
):
  def setOption( extractionOption: ExtractionOption ): ExtractionOptions = extractionOption match
    case ExtractionOption.SetMiner( machine )                              => copy( minerClass = machine )
    case ExtractionOption.SetClockSpeed( clockSpeed )                      => copy( clockSpeed = clockSpeed )
    case ExtractionOption.ToggleExcludeWaterPumpFromOverclocking( enable ) =>
      copy( excludeWaterPumpFromOverclocking = enable )
    case ExtractionOption.ToggleExtractorType( extractor, value ) =>
      copy( extractors = toggle( extractors, extractor, value ) )
    case ExtractionOption.ToggleFrackingPreference( item, value ) =>
      copy( preferFracking = toggle( preferFracking, item, value ) )
    case ExtractionOption.SetResourceWeight( item, value ) =>
      value.toIntOption.fold( this ): w =>
        copy( resourceWeightSliders = resourceWeightSliders.updated( item, w ) )

  private def toggle[A]( set: Set[A], key: A, previousValue: String ): Set[A] =
    val wasEnabled: Boolean = previousValue.toBooleanOption.getOrElse( false )
    if ( wasEnabled ) set - key else set + key

  def resources(
      env: Env,
      resourcesDistribs: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]]
  ): Map[ClassName[Item], SolverRequest.Resource] =
    env.game
      .resources( minerClass, clockSpeed, extractors, resourcesDistribs, ResourceWeights( resourceWeightSliders ) )
      .fmap:
        case ( cap, cost ) => SolverRequest.Resource( cap, cost )

object ExtractionOptions:

  def init( game: Model ): ExtractionOptions =
    ExtractionOptions(
      game.machines.values
        .filter( _.machineType.extractor.contains( ExtractorType.Miner ) )
        .maxBy( _.powerConsumption )
        .className,
      ClockSpeedPreset.`100%`,
      true,
      ExtractorType.cases.toSet,
      Set.empty,
      game.extractedItems.map( item => ( item.className, 4 ) ).toMap
    )

  given Conversion[ExtractionOptions, pp.ExtractionOptions]:
    override def apply( x: ExtractionOptions ): pp.ExtractionOptions =
      pp.ExtractionOptions(
        x.minerClass,
        x.clockSpeed,
        x.excludeWaterPumpFromOverclocking,
        x.extractors,
        x.preferFracking,
        x.resourceWeightSliders
      )

  def from( p: pp.ExtractionOptions ): ExtractionOptions =
    ExtractionOptions(
      p.minerClass,
      p.clockSpeed,
      p.excludeWaterPumpFromOverclocking,
      p.extractors,
      p.preferFracking,
      p.resourceWeightSliders
    )
