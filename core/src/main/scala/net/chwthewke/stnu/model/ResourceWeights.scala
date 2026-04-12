package net.chwthewke.stnu
package model

import cats.Eq
import cats.Show
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import scala.collection.immutable.SortedMap

opaque type ResourceWeights = Map[ClassName[Item], Int] /* int coded btw `0` and `2 * range` inclusive */

object ResourceWeights:

  inline def apply( weights: Map[ClassName[Item], Int] ): ResourceWeights = weights

  val default: ResourceWeights = Map.empty

  private val total: Double                      = 1e6
  val range: Int                                 = 8 // weight between 0 (use less) and 2*range (use more) inclusive
  private def weightFactor( input: Int ): Double =
    math.pow( 2d, ( range - input ).toDouble / 4 )

  extension ( resourceWeights: ResourceWeights )
    def weights: Map[ClassName[Item], Int] = resourceWeights

    def costs( resourceCaps: SortedMap[ClassName[Item], Option[Double]] ): Map[ClassName[Item], Double] =
      val capSum: Double = resourceCaps.combineAll.getOrElse( 1d )

      val raw: SortedMap[ClassName[Item], Double] =
        resourceCaps
          .flatMap:
            case ( item, capOpt ) =>
              val cost =
                capOpt match
                  case Some( 0d )  => none
                  case Some( cap ) => ( capSum / cap * weightFactor( resourceWeights.getOrElse( item, range ) ) ).some
                  case None        => 1d.some
              cost.tupleLeft( item )

      val sum: Double = raw.combineAll

      // normalize weights to expected total
      raw.fmap( _ * total / sum )

  given Eq[ResourceWeights]   = Eq[Map[ClassName[Item], Int]]
  given Show[ResourceWeights] = Show.show: weights =>
    weights
      .map:
        case ( item, weight ) => show"$item: $weight"
      .mkString( "\n" )

  given Decoder[ResourceWeights] = Decoder[Map[ClassName[Item], Int]]
  given Encoder[ResourceWeights] = Encoder[Map[ClassName[Item], Int]]
