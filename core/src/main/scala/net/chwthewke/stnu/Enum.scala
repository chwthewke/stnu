package net.chwthewke.stnu

import cats.Eq
import cats.Order
import cats.Show
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

// TODO not really a fan... a lot of this is rehashing enumeratum stuff.
//  use an enum with matching names for source stuff and another one for ourselves?
trait Enum[A]:
  def keyOf( a: A ): String = a.toString

  def values: Array[A]
  lazy val cases: Vector[A] = values.toVector

  private lazy val casesMap: Map[String, A] = cases.fproductLeft( keyOf ).toMap
  private lazy val casesList: String        = cases.map( keyOf ).mkString( ", " )

  def indexOf( a: A ): Int = cases.indexOf( a )

  def withNameOption( name: String ): Option[A] = casesMap.get( name )
  def withNameEither( name: String ): Either[String, A] =
    withNameOption( name ).toRight( s"$name not in $casesList" )

trait CatsEnum[A] extends Enum[A]:
  given Eq[A]   = Eq.by( keyOf )
  given Show[A] = Show.show( keyOf )

trait OrderEnum[A] extends CatsEnum[A]:
  given Order[A]    = Order.by( indexOf )
  given Ordering[A] = Order.catsKernelOrderingForOrder

trait CirceEnum[A] extends Enum[A]:
  given Decoder[A]    = Decoder[String].emap( withNameEither )
  given Encoder[A]    = Encoder[String].contramap( keyOf )
  given KeyDecoder[A] = KeyDecoder.instance[A]( withNameOption )
  given KeyEncoder[A] = KeyEncoder.instance( keyOf )
