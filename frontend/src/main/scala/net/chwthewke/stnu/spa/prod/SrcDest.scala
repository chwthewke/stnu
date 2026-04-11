package net.chwthewke.stnu
package spa.prod

import cats.syntax.all.*

sealed trait SrcDest

object SrcDest:
  sealed trait Src  extends SrcDest
  sealed trait Dest extends SrcDest

  final case class Extract( process: ClockedRecipe ) extends Src
  final case class Step( process: ClockedRecipe )    extends Src with Dest
  case object Input                                  extends Src
  case object Requested                              extends Dest
  case object Byproduct                              extends Dest

  extension ( self: SrcDest )
    def process: Option[ClockedRecipe] = self match
      case Extract( process )            => process.some
      case Step( process )               => process.some
      case Input | Requested | Byproduct => none

  extension [A <: SrcDest]( self: A )
    def modifyProcess( f: ClockedRecipe => ClockedRecipe ): A =
      ( self match
        case Extract( process )            => Extract( f( process ) )
        case Step( process )               => Step( f( process ) )
        case Input | Requested | Byproduct => self
      ).asInstanceOf[A] // NOTE ugly, but that's what it is (and safe)
