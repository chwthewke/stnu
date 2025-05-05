package net.chwthewke.stnu
package protocol
package game

import cats.Show
import cats.derived.strict.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.Model
import model.IconIndex

case class FullModel( game: Model, icons: IconIndex ) derives Show, ConfiguredDecoder, ConfiguredEncoder
