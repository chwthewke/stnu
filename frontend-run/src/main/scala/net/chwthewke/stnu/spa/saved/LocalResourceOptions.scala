package net.chwthewke.stnu
package spa
package saved

import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.ExtractorType
import model.Item
import model.ResourceDistrib
import model.ResourcePurity
import spa.plan.ResourceOptionsInputModel

object LocalResourceOptions:
  case class Saved(
      nodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
      inputs: Vector[( ( ExtractorType, ClassName[Item], ResourcePurity ), Option[String] )]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( resourceOptions: ResourceOptionsInputModel ): Saved =
      Saved(
        resourceOptions.resourceNodes,
        resourceOptions.inputs.fmap( fromInputModel ).toVector
      )

  case class Loaded(
      nodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
      inputs: Vector[( ( ExtractorType, ClassName[Item], ResourcePurity ), Option[String] )]
  ) derives ConfiguredDecoder:
    def toResourceOptions: ResourceOptionsInputModel =
      ResourceOptionsInputModel(
        nodes,
        inputs.toMap.fmap( toInputModel )
      )
