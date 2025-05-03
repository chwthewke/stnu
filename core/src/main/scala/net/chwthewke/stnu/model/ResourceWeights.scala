package net.chwthewke.stnu
package model

import cats.Eq
import cats.Show
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder

opaque type ResourceWeights = Map[ClassName[Item], Int] /* int coded btw `0` and `2 * range` inclusive */

object ResourceWeights:

  inline def apply( weights: Map[ClassName[Item], Int] ): ResourceWeights = weights

  val default: ResourceWeights = Map.empty

  private val total: Double = 1e6
  val range: Int            = 4 // weight between -range and range inclusive

  extension ( resourceWeights: ResourceWeights )
    def weights: Map[ClassName[Item], Int] = resourceWeights

    def costs( resourceCaps: Map[Item, Double] ): Map[Item, Double] =
      val raw = resourceCaps.map:
        case ( item, cap ) =>
          (
            item,
            1d / math.max( cap, 1e-5 ) *
              math.pow( 2d, ( resourceWeights.getOrElse( item.className, range ) - range ).toDouble / 4d )
          )

      val sum = raw.values.sum

      raw.map:
        case ( item, rawCap ) => ( item, total * rawCap / sum )

  given Eq[ResourceWeights] = Eq[Map[ClassName[Item], Int]]
  given Show[ResourceWeights] = Show.show: weights =>
    weights
      .map:
        case ( item, weight ) => show"$item: $weight"
      .mkString( "\n" )

  given Decoder[ResourceWeights] = Decoder[Map[ClassName[Item], Int]]
  given Encoder[ResourceWeights] = Encoder[Map[ClassName[Item], Int]]
