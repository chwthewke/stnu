package net.chwthewke.stnu
package protocol.solver

import cats.syntax.all.*
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

def retry[A]( g: Gen[A], n: Int = 20 ): Gen[A] =
  ( none[Int], n )
    .tailRecM:
      case ( acc, r ) =>
        ( Gen.choose( Int.MinValue, Int.MaxValue ), g.map( _.some ) <+> Gen.const( none ) ).tupled.map:
          case ( _, Some( v ) ) => v.some.asRight
          case ( x, _ )         =>
            if ( r == 0 )
              none.asRight
            else
              ( x.some, r - 1 ).asLeft
    .map:
      case Some( v ) => v
      case None      => throw Gen.RetryUntilException( n )
