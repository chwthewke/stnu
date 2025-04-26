package net.chwthewke.stnu
package model

import cats.Show
import io.circe.Decoder
import io.circe.Encoder
import scala.collection.immutable.SortedSet

opaque type ModelIndex = SortedSet[ModelVersion]

object ModelIndex:
  inline def apply( versions: Iterable[ModelVersion] ): ModelIndex = versions.to( SortedSet )
  val empty: ModelIndex                                            = SortedSet.empty

  extension ( index: ModelIndex )
    def versions: SortedSet[ModelVersion]        = index
    def add( version: ModelVersion ): ModelIndex = index + version

  given Show[ModelIndex]    = Show[SortedSet[ModelVersion]]
  given Decoder[ModelIndex] = Decoder[SortedSet[ModelVersion]]
  given Encoder[ModelIndex] = Encoder[SortedSet[ModelVersion]]
