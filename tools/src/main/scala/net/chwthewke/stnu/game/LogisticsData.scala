package net.chwthewke.stnu
package game

import cats.Show
import cats.derived.strict.*
import io.circe.Decoder

case class LogisticsData(
    className: ClassName[LogisticsData],
    displayName: String,
    amountPerMinute: Int
) derives Show

object LogisticsData:
  private given Decoder[Double] = Decoders.doubleStringDecoder

  opaque type ConveyorBelt = LogisticsData
  object ConveyorBelt:
    inline def apply( className: ClassName[LogisticsData], displayName: String, speed: Double ): ConveyorBelt =
      LogisticsData( className, displayName, ( speed / 2 ).toInt )
    extension ( self: ConveyorBelt ) def data: LogisticsData = self

    given Show[ConveyorBelt]    = Show[LogisticsData]
    given Decoder[ConveyorBelt] = Decoder.forProduct3( "ClassName", "mDisplayName", "mSpeed" )( ConveyorBelt.apply )

  opaque type Pipeline = LogisticsData
  object Pipeline:
    inline def apply( className: ClassName[LogisticsData], displayName: String, flowLimit: Double ): Pipeline =
      LogisticsData( className, displayName, flowLimit.toInt * 60 )
    extension ( self: Pipeline ) def data: LogisticsData = self

    given Show[Pipeline]    = Show[LogisticsData]
    given Decoder[Pipeline] = Decoder.forProduct3( "ClassName", "mDisplayName", "mFlowLimit" )( Pipeline.apply )
