package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.Machine

case class PowerOptions(
    allowedGenerators: Set[ClassName[Machine]]
) derives ConfiguredCodec
