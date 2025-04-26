package net.chwthewke.stnu
package data

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder

opaque type ImageName = String

object ImageName:
  inline def apply( name: String ): ImageName         = name
  extension ( imageName: ImageName ) def name: String = imageName

  given Show[ImageName]    = Show[String]
  given Order[ImageName]   = Order[String]
  given Decoder[ImageName] = Decoder[String]
  given Encoder[ImageName] = Encoder[String]
