package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*

import model.ExtractorType
import model.Item
import model.ResourceDistrib
import model.ResourceOptions
import model.ResourcePurity

case class ResourceOptionsInputModel(
    resourceNodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
    inputs: Map[( ExtractorType, ClassName[Item], ResourcePurity ), InputModel]
):
  def restore: ResourceOptionsInputModel = copy( inputs = inputs.fmap( _.restore ) )
  def setResourceDistribution(
      extractor: ExtractorType,
      item: ClassName[Item],
      purity: ResourcePurity,
      value: String
  ): ResourceOptionsInputModel =
    value.toIntOption.fold( this )( v =>
      ResourceOptionsInputModel(
        resourceNodes = resourceNodes
          .updatedWith( extractor )( _.map( _.updatedWith( item )( _.map( _.set( purity, v ) ) ) ) ),
        inputs.updatedWith( ( extractor, item, purity ) )( _.map( _ => InputModel.onInput( value ) ) )
      )
    )

object ResourceOptionsInputModel:
  def defaultInputs( defaults: ResourceOptions ): Map[( ExtractorType, ClassName[Item], ResourcePurity ), InputModel] =
    for
      ( ex, m ) <- defaults.resourceNodes
      ( it, d ) <- m
      ( p, v )  <- ResourcePurity.cases.fproduct( d.get )
    yield ( ( ex, it, p ), InputModel.withDefault( v.toString ) )

  def init( defaults: ResourceOptions ): ResourceOptionsInputModel =
    // NOTE the defaults have an arbitrary high value for water/water pump "resource nodes", which is not
    //   editable. This gives us a very high cap on water when water pump is allowed.
    ResourceOptionsInputModel(
      defaults.resourceNodes,
      defaultInputs( defaults )
    )

  given Conversion[ResourceOptionsInputModel, pp.ResourceOptions]:
    override def apply( x: ResourceOptionsInputModel ): pp.ResourceOptions =
      pp.ResourceOptions( x.resourceNodes )

  def from( env: Env, p: pp.ResourceOptions ): ResourceOptionsInputModel =
    ResourceOptionsInputModel( p.resourceNodes, defaultInputs( env.game.defaultResourceOptions ) )
