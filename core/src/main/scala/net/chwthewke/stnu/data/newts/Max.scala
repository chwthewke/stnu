package net.chwthewke.stnu
package data
package newts

import cats.CommutativeMonad
import cats.Order
import cats.kernel.CommutativeSemigroup
import scala.annotation.tailrec

opaque type Max[A] = A

object Max:
  inline def apply[A]( value: A ): Max[A]     = value
  extension [A]( self: Max[A] ) def getMax: A = self

  given CommutativeMonad[Max]:
    override def pure[A]( x: A ): Max[A] = x

    override def flatMap[A, B]( fa: Max[A] )( f: A => Max[B] ): Max[B] = f( fa )

    @tailrec
    override final def tailRecM[A, B]( a: A )( f: A => Max[Either[A, B]] ): Max[B] =
      f( a ) match
        case Left( a1 ) => tailRecM( a1 )( f )
        case Right( b ) => b

  given [A] => ( ord: Order[A] ) => CommutativeSemigroup[Max[A]] with Order[Max[A]]:
    override def compare( x: Max[A], y: Max[A] ): Int    = ord.compare( x, y )
    override def combine( x: Max[A], y: Max[A] ): Max[A] = ord.max( x, y )

  given [A] => Order[Max[A]] => Ordering[Max[A]] = Order.catsKernelOrderingForOrder[Max[A]]
