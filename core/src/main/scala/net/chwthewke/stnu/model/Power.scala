package net.chwthewke.stnu
package model

import cats.Monoid
import cats.Show
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

enum Power derives ConfiguredDecoder, ConfiguredEncoder:
  case Production( value: Double )          extends Power
  case Fixed( value: Double )               extends Power with Power.Consumption_
  case Variable( min: Double, max: Double ) extends Power with Power.Consumption_

object Power:
  extension ( power: Power )
    def average: Double = power match
      case Production( value )  => -value
      case Fixed( value )       => value
      case Variable( min, max ) => ( min + max ) / 2
    def min: Double = power match
      case Production( value ) => -value
      case Fixed( value )      => value
      case Variable( min, _ )  => min
    def max: Double = power match
      case Production( value ) => -value
      case Fixed( value )      => value
      case Variable( _, max )  => max

    def map( consumed: Double => Double, produced: Double => Double ): Power = power match
      case Production( value )  => Production( produced( value ) )
      case Fixed( value )       => Fixed( consumed( value ) )
      case Variable( min, max ) => Variable( consumed( min ), consumed( max ) )

  extension ( powerConsumption: Power.Consumption )
    def combine( other: Power.Consumption ): Power.Consumption = powerConsumption match
      case Fixed( value ) =>
        other match
          case Fixed( otherValue )  => Fixed( value + otherValue )
          case Variable( min, max ) => Variable( value + min, value + max )
      case Variable( min, max ) =>
        Variable( min + other.min, max + other.max )

  sealed trait Consumption_

  type Consumption = Power & Consumption_

  given Monoid[Power.Consumption]:
    override def empty: Power.Consumption = Fixed( 0d )

    override def combine( x: Power.Consumption, y: Power.Consumption ): Power.Consumption = x.combine( y )

  given Show[Power] = Show.show:
    case Production( value )  => f"$value% 8.2f"
    case Fixed( value )       => f"$value% 6.2f"
    case Variable( min, max ) => f"$min%6.2f-$max%6.2f"
