package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.Item
import model.prod.FlowEnd
import model.prod.Group

case class Flows(
    prodHash: Int,
    nextId: ProcessSplitId,
    endSplits: Vector[( EndId, Vector[( ProcessSplitId, Double /* fraction */, Group )] )],
    itemFlows: Map[ClassName[Item], Vector[Vector[( FlowEnd, ProcessSplitId )]]]
) derives ConfiguredCodec
