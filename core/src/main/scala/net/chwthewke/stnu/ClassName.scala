package net.chwthewke.stnu

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

opaque type ClassName = String

object ClassName:
  inline def apply( name: String ): ClassName = name
  extension ( className: ClassName )
    def name: String = className
    def buildingDescriptor: Option[ClassName] =
      Option.when( name.startsWith( buildingPrefix ) )( descriptorPrefix + name.stripPrefix( buildingPrefix ) )

  private val buildingPrefix: String   = "Build_"
  private val descriptorPrefix: String = "Desc_"

  given Show[ClassName]       = Show[String]
  given Order[ClassName]      = Order[String]
  given Ordering[ClassName]   = Order.catsKernelOrderingForOrder
  given Decoder[ClassName]    = Decoder[String]
  given Encoder[ClassName]    = Encoder[String]
  given KeyDecoder[ClassName] = KeyDecoder[String]
  given KeyEncoder[ClassName] = KeyEncoder[String]
