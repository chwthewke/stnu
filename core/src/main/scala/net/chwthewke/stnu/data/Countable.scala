package net.chwthewke.stnu
package data

import cats.Applicative
import cats.Bitraverse
import cats.Eval
import cats.Monad
import cats.Order
import cats.Show
import cats.Traverse
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder
import scala.annotation.tailrec
import scala.collection.Factory

final case class Countable[+N, +A]( item: A, amount: N ) derives Order:
  def mapAmount[M]( f: N => M ): Countable[M, A] = copy( amount = f( amount ) )
  def withAmount[M]( a: M ): Countable[M, A]     = copy( amount = a )

object Countable:
  extension [F[x] <: Iterable[x], N, A]( self: F[Countable[N, A]] )
    def gather( using N: Numeric[N], F: Factory[Countable[N, A], F[Countable[N, A]]] ): F[Countable[N, A]] =
      self
        .groupMap( _.item )( _.amount )
        .map:
          case ( item, amounts ) => Countable( item, amounts.sum )
        .to( F )

  given [N: Numeric] => Monad[Countable[N, *]] & Traverse[Countable[N, *]] = new CountableInstance[N]

  private class CountableInstance[N]( using N: Numeric[N] )
      extends Monad[Countable[N, *]]
      with Traverse[Countable[N, *]]:
    def pure[A]( x: A ): Countable[N, A] = Countable( x, N.one )

    def flatMap[A, B]( fa: Countable[N, A] )( f: A => Countable[N, B] ): Countable[N, B] =
      val Countable( b, m ) = f( fa.item )
      Countable( b, N.times( m, fa.amount ) )

    def tailRecM[A, B]( a: A )( f: A => Countable[N, Either[A, B]] ): Countable[N, B] =
      @tailrec
      def loop( acc: N, a0: A ): Countable[N, B] =
        f( a0 ) match
          case Countable( Left( a1 ), n )  => loop( N.times( n, acc ), a1 )
          case Countable( Right( a1 ), n ) => Countable( a1, N.times( n, acc ) )

      loop( N.one, a )

    def traverse[G[_], A, B]( fa: Countable[N, A] )( f: A => G[B] )( using G: Applicative[G] ): G[Countable[N, B]] =
      G.map( f( fa.item ) )( Countable( _, fa.amount ) )

    def foldLeft[A, B]( fa: Countable[N, A], b: B )( f: ( B, A ) => B ): B =
      f( b, fa.item )

    def foldRight[A, B]( fa: Countable[N, A], lb: Eval[B] )( f: ( A, Eval[B] ) => Eval[B] ): Eval[B] =
      f( fa.item, lb )

  given Bitraverse[Countable]:
    override def leftMap[A, B, C]( fab: Countable[A, B] )( f: A => C ): Countable[C, B] = fab.mapAmount( f )

    override def bimap[A, B, C, D]( fab: Countable[A, B] )( f: A => C, g: B => D ): Countable[C, D] =
      Countable( g( fab.item ), f( fab.amount ) )

    override def bitraverse[G[_]: Applicative, A, B, C, D](
        fab: Countable[A, B]
    )( f: A => G[C], g: B => G[D] ): G[Countable[C, D]] =
      ( g( fab.item ), f( fab.amount ) ).mapN( Countable( _, _ ) )

    override def bifoldLeft[A, B, C]( fab: Countable[A, B], c: C )( f: ( C, A ) => C, g: ( C, B ) => C ): C =
      f( g( c, fab.item ), fab.amount )

    override def bifoldRight[A, B, C]( fab: Countable[A, B], c: Eval[C] )(
        f: ( A, Eval[C] ) => Eval[C],
        g: ( B, Eval[C] ) => Eval[C]
    ): Eval[C] =
      f( fab.amount, g( fab.item, c ) )

  given [N: Show, A: Show] => Show[Countable[N, A]] =
    Show.show:
      case Countable( name, amount ) => show"$name ($amount)"

  given [N: Order, A: Order] => Ordering[Countable[N, A]] = Order.catsKernelOrderingForOrder

  given [N: Decoder, A: Decoder] => Decoder[Countable[N, A]] = ConfiguredDecoder.derived[Countable[N, A]]
  given [N: Encoder, A: Encoder] => Encoder[Countable[N, A]] = ConfiguredEncoder.derived[Countable[N, A]]
