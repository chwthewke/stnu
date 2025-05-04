package net.chwthewke.stnu
package game

import cats.Show
import cats.derived.strict.*
import io.circe.Decoder
import scala.concurrent.duration.*

case class SimpleProducer(
    className: ClassName[SimpleProducer],
    displayName: String,
    timeToProduceItem: FiniteDuration
) derives Show

object SimpleProducer:
  val knownSimpleProducers: Map[ClassName[SimpleProducer], ClassName[GameItem]] =
    Map( "Build_TreeGiftProducer_C" -> "Desc_Gift_C" )
      .map:
        case ( k, v ) => ( ClassName( k ), ClassName( v ) )

  given Decoder[SimpleProducer] =
    Decoder.forProduct3( "ClassName", "mDisplayName", "mTimeToProduceItem" )(
      ( cn: ClassName[SimpleProducer], dn: String, ttpi: FiniteDuration ) => SimpleProducer( cn, dn, ttpi )
    )( Decoder[ClassName[SimpleProducer]], Decoder[String], Decoders.doubleStringDecoder.map( _.seconds ) )
