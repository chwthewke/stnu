package net.chwthewke.stnu
package spa
package plan

import cats.data.Ior
import cats.syntax.all.*

import model.ExtractionRecipes
import model.ExtractorType
import model.Item
import model.Machine
import model.Model
import model.ResourceWeights
import protocol.solver.SolverRequest
import spa.prod.ClockSpeedPreset

case class ExtractionOptions(
    minerClass: ClassName[Machine],
    clockSpeed: ClockSpeedPreset,
    extractors: Set[ExtractorType],
    preferFracking: Set[ClassName[Item]],
    resourceWeightSliders: Map[ClassName[Item], Int]
):
  def setOption( extractionOption: ExtractionOption ): ExtractionOptions = extractionOption match
    case ExtractionOption.SetMiner( machine )                     => copy( minerClass = machine )
    case ExtractionOption.SetClockSpeed( clockSpeed )             => copy( clockSpeed = clockSpeed )
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

  private def resourceCaps( env: Env, resources: ResourceOptionsInputModel ): Map[ClassName[Item], Double] =
    def getExtractionRecipes( machineFilter: Machine => Boolean ): Map[ClassName[Item], ExtractionRecipes] =
      env.game.extractionRecipes
        .flatMap:
          case ( ( item, machine ), recipes ) =>
            Option.when( machineFilter( machine ) )( ( item.className, recipes ) )

    resources.resourceNodes.toVector.foldMap:
      case ( extractor, distribs ) =>
        val extractorRecipes: Map[ClassName[Item], ExtractionRecipes] = extractor match
          case ExtractorType.Miner =>
            getExtractionRecipes( _.className == minerClass )
          case other =>
            getExtractionRecipes( _.machineType.extractor.contains( other ) && extractors.contains( other ) )

        extractorRecipes.alignWith( distribs ) {
          case Ior.Both( ExtractionRecipes.Variable( byPurity ), distrib ) =>
            clockSpeed.value.toDouble / 100d *
              distrib.foldMap( ( purity, count ) => count * byPurity.get( purity ).productsPerMinute.amount )
          case _ => 0d
        }

  def resources( env: Env, resourceOptions: ResourceOptionsInputModel ): Map[ClassName[Item], SolverRequest.Resource] =
    val caps  = resourceCaps( env, resourceOptions )
    val costs = ResourceWeights( resourceWeightSliders ).costs( caps )
    caps.map:
      case ( item, cap ) =>
        ( item, SolverRequest.Resource( cap, costs.getOrElse( item, 1d ) ) )

object ExtractionOptions:

  def init( game: Model ): ExtractionOptions =
    ExtractionOptions(
      game.machines.values
        .filter( _.machineType.extractor.contains( ExtractorType.Miner ) )
        .maxBy( _.powerConsumption )
        .className,
      ClockSpeedPreset.`100%`,
      ExtractorType.cases.toSet,
      Set.empty,
      game.extractedItems.map( item => ( item.className, 4 ) ).toMap
    )
