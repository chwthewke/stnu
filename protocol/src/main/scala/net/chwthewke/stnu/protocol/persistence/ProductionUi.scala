package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

case class ProductionUi(
    productionRowOrder: Option[Vector[Int]]
) derives ConfiguredCodec
