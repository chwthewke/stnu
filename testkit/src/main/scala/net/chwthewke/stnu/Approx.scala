package net.chwthewke.stnu

import cats.data.Ior
import cats.data.NonEmptyVector
import cats.syntax.all.*

import data.Countable

trait Approx[-A]:
  def approx( x: A, y: A ): Boolean

extension [A: Approx]( x: A ) def =~( y: A ): Boolean = summon[Approx[A]].approx( x, y )

trait ApproxLower:
  given approxEq[A]: Approx[A] = new Approx:
    override def approx( x: A, y: A ): Boolean = x == y

trait ApproxLow extends ApproxLower:
  given [A: Approx] => Approx[Vector[A]]:
    override def approx( xs: Vector[A], ys: Vector[A] ): Boolean =
      xs.length == ys.length && xs.zip( ys ).forall { case ( x, y ) => x =~ y }

  given [A: Approx] => Approx[Option[A]]:
    override def approx( x: Option[A], y: Option[A] ): Boolean =
      ( x, y ).mapN( _ =~ _ ).getOrElse( x == y )

  given [K, V: Approx] => Approx[Map[K, V]]:
    override def approx( x: Map[K, V], y: Map[K, V] ): Boolean =
      x.align( y )
        .values
        .forall:
          case Ior.Both( a, b ) => a =~ b
          case _                => false

  given [A: Approx] => Approx[NonEmptyVector[A]]:
    override def approx( xs: NonEmptyVector[A], ys: NonEmptyVector[A] ): Boolean =
      xs.length == ys.length
        && xs.zipWith( ys )( _ =~ _ ).forall( identity )

object Approx extends ApproxLow:

  given Approx[Double]:
    override def approx( x: Double, y: Double ): Boolean = ( x - y ).abs < Countable.Tolerance

  given [A: Approx, B: Approx] => Approx[( A, B )]:
    override def approx( x: ( A, B ), y: ( A, B ) ): Boolean =
      x._1 =~ y._1
        && x._2 =~ y._2

  given [A: Approx, B: Approx, C: Approx] => Approx[( A, B, C )]:
    override def approx( x: ( A, B, C ), y: ( A, B, C ) ): Boolean =
      x._1 =~ y._1
        && x._2 =~ y._2
        && x._3 =~ y._3

  given [A: Approx, B: Approx, C: Approx, D: Approx] => Approx[( A, B, C, D )]:
    override def approx( x: ( A, B, C, D ), y: ( A, B, C, D ) ): Boolean =
      x._1 =~ y._1
        && x._2 =~ y._2
        && x._3 =~ y._3
        && x._4 =~ y._4

  given [A: Approx] => Approx[Countable[Double, A]]:
    override def approx( x: Countable[Double, A], y: Countable[Double, A] ): Boolean =
      x.item =~ y.item
        && x.amount =~ y.amount
