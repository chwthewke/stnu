package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec
import scala.collection.immutable.SortedMap

import model.Item

case class RequestSelection(
    requestedAmounts: SortedMap[ClassName[Item], Double]
) derives ConfiguredCodec
