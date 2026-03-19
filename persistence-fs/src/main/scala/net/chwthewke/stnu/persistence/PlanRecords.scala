package net.chwthewke.stnu
package persistence

import fs2.io.file.Path
import scala.collection.immutable.SortedMap
import scodec.Codec

import protocol.persistence.PlanId

case class PlanRecords( plans: SortedMap[PlanId, Path] )

object PlanRecords:
  given Codec[PlanRecords] =
    import scodec.codecs.*
    vectorOfN[( PlanId, Path )]( int32, Codecs.planId :: Codecs.path )
      .xmap( v => PlanRecords( v.to( SortedMap ) ), _.plans.toVector )
