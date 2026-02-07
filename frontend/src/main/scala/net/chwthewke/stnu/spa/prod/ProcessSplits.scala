package net.chwthewke.stnu
package spa
package prod

import scala.collection.immutable.SortedMap

import model.prod.Group
import protocol.persistence.ProcessSplitId

case class ProcessSplits( splits: SortedMap[ProcessSplitId, ( Double, Group )] )

object ProcessSplits:
  def init( id: ProcessSplitId ): ProcessSplits =
    ProcessSplits( SortedMap( id -> ( 1d, Group.root ) ) )
