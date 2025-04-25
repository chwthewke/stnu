package net.chwthewke.stnu
package game

import io.circe.Decoder
import scala.util.Try

object Decoders:
  val doubleStringDecoder: Decoder[Double]   = Decoder[String].emapTry( s => Try( s.toDouble ) )
  val intStringDecoder: Decoder[Int]         = Decoder[String].emapTry( s => Try( s.toInt ) )
  val booleanStringDecoder: Decoder[Boolean] = Decoder[String].emapTry( s => Try( s.toBoolean ) )
