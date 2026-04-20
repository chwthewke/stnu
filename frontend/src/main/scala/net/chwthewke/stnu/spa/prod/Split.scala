package net.chwthewke.stnu
package spa
package prod

import cats.syntax.all.*

import model.prod.Group
import protocol.persistence.ProcessSplitId

// NOTE for some reason I chose 1 <= split <= max
case class Split[+A <: SrcDest](
    id: ProcessSplitId,
    end: EndId,
    original: A,
    number: Int,
    max: Int,
    fraction: Double,
    group: Group
):
  val value: A                            = original.mapProcess( _.times( fraction ) )
  def times( fraction: Double ): Split[A] = copy( fraction = this.fraction * fraction )
