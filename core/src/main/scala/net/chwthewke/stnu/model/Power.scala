package net.chwthewke.stnu
package model

import cats.Monoid
import cats.Show
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

enum Power derives ConfiguredDecoder, ConfiguredEncoder:
  case Fixed( value: Double )               extends Power
  case Variable( min: Double, max: Double ) extends Power

object Power:
  extension ( power: Power )
    def average: Double = power match
      case Fixed( value )       => value
      case Variable( min, max ) => ( min + max ) / 2
    def min: Double = power match
      case Fixed( value )       => value
      case Variable( min, max ) => min
    def max: Double = power match
      case Fixed( value )       => value
      case Variable( min, max ) => max
    def map( f: Double => Double ): Power = power match
      case Fixed( value )       => Fixed( f( value ) )
      case Variable( min, max ) => Variable( f( min ), f( max ) )
    def combine( other: Power ): Power = power match
      case Fixed( value ) =>
        other match
          case Fixed( otherValue )  => Fixed( value + otherValue )
          case Variable( min, max ) => Variable( value + min, value + max )
      case Variable( min, max ) =>
        Variable( min + other.min, max + other.max )

  given Monoid[Power]:
    override def empty: Power = Fixed( 0d )

    override def combine( x: Power, y: Power ): Power = x.combine( y )

  given Show[Power] = Show.show:
    case Fixed( value )       => f"$value% 6.2f"
    case Variable( min, max ) => f"$min%6.2f-$max%6.2f"
