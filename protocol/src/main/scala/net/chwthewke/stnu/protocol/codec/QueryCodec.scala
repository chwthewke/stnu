package net.chwthewke.stnu
package protocol
package codec

import cats.syntax.all.*
import org.http4s.QueryParamDecoder
import org.http4s.QueryParamEncoder
import org.http4s.QueryParameterValue

trait QueryCodec[A]:
  self =>

  def apply( a: A ): Map[String, Vector[String]]

  def unapply( queryParams: Map[String, collection.Seq[String]] ): Option[A]

  final def imap[B]( f: A => B, g: B => A ): QueryCodec[B] =
    new QueryCodec[B]:
      override def apply( b: B ): Map[String, Vector[String]] = self.apply( g( b ) )

      override def unapply( queryParams: Map[String, collection.Seq[String]] ): Option[B] =
        self.unapply( queryParams ).map( f )

  final def imapFilter[B]( f: A => Option[B], g: B => A ): QueryCodec[B] =
    new QueryCodec[B]:
      override def apply( b: B ): Map[String, Vector[String]] = self.apply( g( b ) )

      override def unapply( queryParams: Map[String, collection.Seq[String]] ): Option[B] =
        self.unapply( queryParams ).flatMap( f )

  def concat[B]( that: QueryCodec[B] ): QueryCodec[( A, B )] =
    new QueryCodec.Concat( this, that )

  def &+[B]( that: QueryCodec[B] )( using tc: TConcat[A, B] ): QueryCodec[tc.Out] =
    concat( that ).imap( tc.concat.tupled, tc.expand )

object QueryCodec:
  private[codec] object Empty extends QueryCodec[Unit]:
    override def apply( a: Unit ): Map[String, Vector[String]] = Map.empty

    override def unapply( queryParams: Map[String, collection.Seq[String]] ): Option[Unit] = Some( () )

  class Concat[A, B]( private val lhs: QueryCodec[A], private val rhs: QueryCodec[B] ) extends QueryCodec[( A, B )]:
    override def apply( a: ( A, B ) ): Map[String, Vector[String]] =
      lhs.apply( a._1 ) |+| rhs.apply( a._2 )

    override def unapply( queryParams: Map[String, collection.Seq[String]] ): Option[( A, B )] =
      ( lhs.unapply( queryParams ), rhs.unapply( queryParams ) ).tupled

  private def fromQueryParamCodec[A](
      name: String
  )( using enc: QueryParamEncoder[A], dec: QueryParamDecoder[A] ): QueryCodec[Option[Vector[A]]] =
    new QueryCodec[Option[Vector[A]]]:
      override def apply( a: Option[Vector[A]] ): Map[String, Vector[String]] =
        a.foldMap( xs => Map( name -> xs.map( x => enc.encode( x ).value ) ) )

      override def unapply( queryParams: Map[String, collection.Seq[String]] ): Option[Option[Vector[A]]] =
        queryParams
          .get( name )
          .traverse: values =>
            values.toVector
              .traverse: value =>
                dec.decode( QueryParameterValue( value ) ).toOption

  def multipleOpt[A: {QueryParamEncoder, QueryParamDecoder}]( name: String ): QueryCodec[Option[Vector[A]]] =
    fromQueryParamCodec( name )
  def multiple[A: {QueryParamEncoder, QueryParamDecoder}]( name: String ): QueryCodec[Vector[A]] =
    fromQueryParamCodec( name ).imapFilter( identity, _.some )
  def singleOpt[A: {QueryParamEncoder, QueryParamDecoder}]( name: String ): QueryCodec[Option[A]] =
    fromQueryParamCodec( name ).imapFilter(
      _.traverse( xs => xs.headOption.filter( _ => xs.length == 1 ) ),
      _.map( Vector( _ ) )
    )
  def single[A: {QueryParamEncoder, QueryParamDecoder}]( name: String ): QueryCodec[A] =
    fromQueryParamCodec( name ).imapFilter(
      _.flatMap( xs => xs.headOption.filter( _ => xs.length == 1 ) ),
      a => Vector( a ).some
    )
