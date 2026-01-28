package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.ExtractorType
import model.Item
import model.ResourceDistrib

case class ResourceOptions(
    resourceNodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]]
) derives ConfiguredCodec
