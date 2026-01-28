package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec
import scala.collection.immutable.SortedSet

import model.Transport

case class LogisticsOptions(
    singleBelt: Option[ClassName[Transport]],
    singlePipeline: Option[ClassName[Transport]],
    allBelts: SortedSet[ClassName[Transport]],
    allPipelines: SortedSet[ClassName[Transport]]
) derives ConfiguredCodec
