package net.chwthewke.stnu
package debug

import cats.Eval
import scala.annotation.tailrec

enum Data:
  case Prim( tag: String, eager: Boolean, value: Eval[String] )
  case Tuple( elements: Vector[Eval[Data]] )
  case Prod( tag: String, members: Vector[( String, Eval[Data] )] )
  case Coprod( tag: String, value: Eval[Data] )
  case Opaque( desc: String )
  case Coll( tag: String, ordered: Boolean, elements: Data.Lazy )
  case Map( tag: String, ordered: Boolean, elements: Vector[Eval[( Data, Eval[Data] )]] )

object Data:
  case class Lazy( elements: Vector[Eval[Data]], tail: Option[Eval[Lazy]] ):
    final def force: Lazy = tail match
      case None           => this
      case Some( tailEv ) =>
        val tailV = tailEv.value
        Lazy( elements ++ tailV.elements, tailV.tail )
    @tailrec
    final def forceAll: Vector[Data] =
      tail match
        case None      => elements.map( _.value )
        case Some( _ ) => force.forceAll

  object Lazy:
    def apply( elements: Vector[Eval[Data]] ): Lazy = Lazy( elements, None )
    def empty: Lazy                                 = Lazy( Vector.empty, None )

  extension ( self: Data )
    def tag: String = self match
      case Data.Prim( tag, _, _ ) => tag
      case Data.Tuple( elements ) => s"Tuple[${elements.size}]"
      case Data.Prod( tag, _ )    => tag
      case Data.Coprod( tag, _ )  => tag
      case Data.Opaque( _ )       => "Opaque"
      case Data.Coll( tag, _, _ ) => tag
      case Data.Map( tag, _, _ )  => tag
