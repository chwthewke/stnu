package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.prod.FlowEnd

case class ItemFlows(
    itemTransports: Vector[Vector[( FlowEnd, ProcessSplitId )]],
    transportSplits: Vector[( Double, Int, Int )]
) derives ConfiguredCodec
