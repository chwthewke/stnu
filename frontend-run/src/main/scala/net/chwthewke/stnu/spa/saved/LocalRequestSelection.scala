package net.chwthewke.stnu
package spa
package saved

import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.Item
import spa.plan.RequestsModel

object LocalRequestSelection:
  case class Saved(
      items: List[( Item, Option[String] )],
      search: Option[String]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( requests: RequestsModel ): Saved =
      Saved( requests.requestAmountEditors.map( _.map( fromInputModel ) ), fromSearchQuery( requests.search ) )

  case class Loaded(
      items: List[( Item, Option[String] )],
      search: Option[String]
  ) derives ConfiguredDecoder:
    def toRequests: RequestsModel =
      RequestsModel( items.map( _.map( toInputModel ) ), toSearchQuery( search ) )
