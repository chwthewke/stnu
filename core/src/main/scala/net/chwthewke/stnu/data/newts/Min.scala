package net.chwthewke.stnu
package data
package newts

import cats.CommutativeMonad
import cats.Order
import cats.kernel.CommutativeSemigroup
import scala.annotation.tailrec

opaque type Min[A] = A

object Min:
  inline def apply[A]( value: A ): Min[A]     = value
  extension [A]( self: Min[A] ) def getMin: A = self

  given CommutativeMonad[Min]:
    override def pure[A]( x: A ): Min[A] = x

    override def flatMap[A, B]( fa: Min[A] )( f: A => Min[B] ): Min[B] = f( fa )

    @tailrec
    override final def tailRecM[A, B]( a: A )( f: A => Min[Either[A, B]] ): Min[B] =
      f( a ) match
        case Left( a1 ) => tailRecM( a1 )( f )
        case Right( b ) => b

  given [A] => ( ord: Order[A] ) => CommutativeSemigroup[Min[A]] with Order[Min[A]]:
    override def compare( x: Min[A], y: Min[A] ): Int    = ord.compare( x, y )
    override def combine( x: Min[A], y: Min[A] ): Min[A] = ord.min( x, y )

  given [A] => Order[Min[A]] => Ordering[Min[A]] = Order.catsKernelOrderingForOrder[Min[A]]
