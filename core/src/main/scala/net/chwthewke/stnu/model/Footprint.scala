package net.chwthewke.stnu
package model

import cats.Show
import io.circe.derivation.ConfiguredCodec

case class Footprint( length: Int, width: Int ) derives ConfiguredCodec

object Footprint:
  given Show[Footprint] = f => f"${f.length / 100d}%fm x ${f.width / 100d}%fm"
