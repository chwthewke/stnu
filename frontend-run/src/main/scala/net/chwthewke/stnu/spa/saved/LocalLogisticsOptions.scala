package net.chwthewke.stnu
package spa
package saved

import cats.data.NonEmptyList
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder
import scala.collection.immutable.SortedSet

import model.Transport
import spa.plan.LogisticsOptions

object LocalLogisticsOptions:
  case class Saved(
      belts: NonEmptyList[ClassName[Transport]],
      pipelines: NonEmptyList[ClassName[Transport]],
      useAll: Boolean
  ) derives ConfiguredEncoder

  object Saved:
    def apply( env: Env, logisticsOptions: LogisticsOptions ): Saved =
      Saved(
        logisticsOptions.belts( env ).map( _.className ),
        logisticsOptions.pipelines( env ).map( _.className ),
        logisticsOptions.useAll
      )

  case class Loaded(
      belts: NonEmptyList[ClassName[Transport]],
      pipelines: NonEmptyList[ClassName[Transport]],
      useAll: Boolean
  ) derives ConfiguredDecoder:
    def toLogisticsOptions: LogisticsOptions =
      LogisticsOptions(
        belts.last,
        pipelines.last,
        belts.toList.to( SortedSet ),
        pipelines.toList.to( SortedSet ),
        useAll
      )
