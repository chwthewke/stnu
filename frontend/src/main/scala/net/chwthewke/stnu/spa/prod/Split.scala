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
    split: Int,
    max: Int,
    fraction: Double,
    group: Group
):
  import Split.*

  val value: A                            = original.times( fraction )
  def times( fraction: Double ): Split[A] = copy( fraction = this.fraction * fraction )

object Split:
  extension [A <: SrcDest]( self: A )
    def times( fraction: Double ): A =
      ( self match
        case SrcDest.Extract( process ) => SrcDest.Extract( process.times( fraction ) )
        case SrcDest.Step( process )    => SrcDest.Step( process.times( fraction ) )
        case SrcDest.Input              => SrcDest.Input
        case SrcDest.Requested          => SrcDest.Requested
        case SrcDest.Byproduct          => SrcDest.Byproduct
      ).asInstanceOf[A]
