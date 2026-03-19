package net.chwthewke.stnu
package protocol
package codec

import scala.Tuple.++
import scala.Tuple.:*

sealed trait TConcat[A, B]:
  type Out
  def concat( a: A, b: B ): Out
  def expand( t: Out ): ( A, B )

object TConcat extends TConcatGiven0:
  def apply[A, B]( using ev: TConcat[A, B] ): TConcat.Aux[A, B, ev.Out] = ev

trait TConcatGiven0 extends TConcatGiven1:
  given tConcatTupleTuple[A <: Tuple, B <: Tuple]( using U: UnConcat.Aux[A ++ B, A, B] ): TConcat.Aux[A, B, A ++ B] =
    instance( ( a, b ) => a ++ b, U.apply )

  given tConcatAnyUnit[A]: TConcat.Aux[A, Unit, A] =
    instance( ( a, _ ) => a, ( _, () ) )

trait TConcatGiven1 extends TConcatGiven2:
  given tConcatAnyTuple[A, B <: Tuple]( using U: UnConcat.Aux[A *: B, Tuple1[A], B] ): TConcat.Aux[A, B, A *: B] =
    instance(
      ( a, b ) => a *: b,
      t =>
        val ( ta, b ) = U.apply( t )
        ( ta._1, b )
    )

  given tConcatUnitAny[A]: TConcat.Aux[Unit, A, A] =
    instance( ( _, a ) => a, ( (), _ ) )

trait TConcatGiven2 extends TConcatGiven3:
  given tConcatTupleAny[A <: Tuple, B]( using U: UnConcat.Aux[A :* B, A, Tuple1[B]] ): TConcat.Aux[A, B, A :* B] =
    instance(
      ( a, b ) => a :* b,
      t =>
        val ( a, tb ) = U.apply( t )
        ( a, tb._1 )
    )

trait TConcatGiven3 extends TConcatFunctions:
  given tConcatDefault[A, B]: TConcat.Aux[A, B, ( A, B )] =
    instance(
      ( a, b ) => ( a, b ),
      t => t
    )

trait TConcatFunctions:
  type Aux[A, B, AB] = TConcat[A, B] { type Out = AB }
  def instance[A, B, AB]( concat0: ( A, B ) => AB, expand0: AB => ( A, B ) ): TConcat.Aux[A, B, AB] =
    new TConcat[A, B]:
      type Out = AB
      override def concat( a: A, b: B ): AB  = concat0( a, b )
      override def expand( t: AB ): ( A, B ) = expand0( t )

  sealed trait UnConcat[S <: Tuple, T <: Tuple]:
    type U <: Tuple
    def apply( s: S ): ( T, U )

  object UnConcat:
    type Aux[S <: Tuple, T <: Tuple, U0] = UnConcat[S, T] { type U = U0 }
    given unConcatEmpty[S <: Tuple]: UnConcat.Aux[S, EmptyTuple, S] =
      new UnConcat[S, EmptyTuple]:
        override type U = S
        override def apply( s: S ): ( EmptyTuple, S ) = ( EmptyTuple, s )

    given unConcatHead[H, S <: Tuple, T <: Tuple]( using UC: UnConcat[S, T] ): UnConcat.Aux[H *: S, H *: T, UC.U] =
      new UnConcat[H *: S, H *: T]:
        override type U = UC.U
        override def apply( hs: H *: S ): ( H *: T, U ) =
          val h *: s   = hs
          val ( t, u ) = UC.apply( s )
          ( h *: t, u )
