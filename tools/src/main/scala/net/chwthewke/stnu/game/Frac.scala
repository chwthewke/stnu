package net.chwthewke.stnu
package game

import scala.annotation.tailrec

case class Frac( num: Long, denom: Long ):
  def +( other: Frac ): Frac = Frac.reduced( num * other.denom + denom * other.num, denom * other.denom )

  def *( other: Frac ): Frac = Frac.reduced( num * other.num, denom * other.denom )

  def inverse: Frac = Frac.reduced( denom, num )

  def /( other: Frac ): Frac = this * other.inverse

  def *( l: Long ): Frac  = Frac.reduced( num * l, denom )
  def *:( l: Long ): Frac = this * l

  def /( l: Long ): Frac  = Frac.reduced( num, denom * l )
  def /:( l: Long ): Frac = l *: inverse

object Frac:
  def reduced( num: Long, denom: Long ): Frac =
    val g: Long = gcd( num, denom )
    Frac( num / g, denom / g )

  def decimal( d: Double ): Frac =
    @tailrec
    def go( decimal: Double, denom: Long ): Frac =
      val num: Long = decimal.toLong
      if ( num.toDouble == decimal ) Frac.reduced( num, denom )
      else go( decimal * 10d, denom * 10 )

    go( d, 1L )

  @tailrec
  def gcd( a: Long, b: Long ): Long = if ( b == 0 ) a else gcd( b, a % b )
