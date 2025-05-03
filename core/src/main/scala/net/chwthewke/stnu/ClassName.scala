package net.chwthewke.stnu

import cats.Order
import cats.Show
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

opaque type ClassName[+A] = String

object ClassName:
  inline def apply[A]( name: String ): ClassName[A] = name
  extension [A]( className: ClassName[A] )
    def name: String            = className
    def narrow[B]: ClassName[B] = className


  given [A] => Show[ClassName[A]]       = Show[String]
  given [A] => Order[ClassName[A]]      = Order[String]
  given [A] => Ordering[ClassName[A]]   = Order.catsKernelOrderingForOrder
  given [A] => Decoder[ClassName[A]]    = Decoder[String]
  given [A] => Encoder[ClassName[A]]    = Encoder[String]
  given [A] => KeyDecoder[ClassName[A]] = KeyDecoder[String]
  given [A] => KeyEncoder[ClassName[A]] = KeyEncoder[String]
