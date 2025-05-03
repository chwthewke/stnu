package net.chwthewke.stnu
package protocol
package codec

import cats.arrow.FunctionK
import cats.syntax.all.*
import org.http4s.Method
import org.http4s.Query
import org.http4s.Request
import org.http4s.dsl.impl.Statuses
import org.http4s.Uri
import org.http4s.dsl.impl.Responses

trait UriCodec[A]:
  self =>

  final def apply( a: A ): Uri = build( a )

  final def unapply( uri: Uri ): Option[A] = extract( uri )

  def apply()( using ev: Unit =:= A ): Uri = apply( ev( () ) )

  def apply[A1, A2]( a1: A1, a2: A2 )( using ev: ( A1, A2 ) <:< A ): Uri =
    apply( ev( ( a1, a2 ) ) )

  def apply[A1, A2, A3]( a1: A1, a2: A2, a3: A3 )( using ev: ( A1, A2, A3 ) <:< A ): Uri =
    apply( ev( ( a1, a2, a3 ) ) )

  def apply[A1, A2, A3, A4]( a1: A1, a2: A2, a3: A3, a4: A4 )( using ev: ( A1, A2, A3, A4 ) <:< A ): Uri =
    apply( ev( ( a1, a2, a3, a4 ) ) )

  def apply[A1, A2, A3, A4, A5]( a1: A1, a2: A2, a3: A3, a4: A4, a5: A5 )( using
      ev: ( A1, A2, A3, A4, A5 ) <:< A
  ): Uri =
    apply( ev( ( a1, a2, a3, a4, a5 ) ) )

  def apply[A1, A2, A3, A4, A5, A6]( a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6 )( using
      ev: ( A1, A2, A3, A4, A5, A6 ) <:< A
  ): Uri =
    apply( ev( ( a1, a2, a3, a4, a5, a6 ) ) )

  def extract( uri: Uri ): Option[A]

  def build( a: A ): Uri

  final def imap[B]( f: A => B, g: B => A ): UriCodec[B] =
    new UriCodec[B]:
      override def extract( uri: Uri ): Option[B] = self.extract( uri ).map( f )

      override def build( b: B ): Uri = self.build( g( b ) )

  final def imapFilter[B]( f: A => Option[B], g: B => A ): UriCodec[B] =
    new UriCodec[B]:
      override def extract( uri: Uri ): Option[B] = self.extract( uri ).flatMap( f )

      override def build( b: B ): Uri = self.build( g( b ) )

  def constant( using ev: A =:= Unit ): UriCodec.Constant =
    new UriCodec.Constant( ev.substituteCo( this ) )

object UriCodec extends UriCodecLowPriority:
  trait Dsl[F[_]] extends Statuses with Responses[F, F]:
    val liftG: FunctionK[F, F] = FunctionK.id[F]
    object `->`:
      def unapply( req: Request[F] ): Some[( Method, Uri )] = Some( ( req.method, req.uri ) )

  class Default[P, Q, A](
      private val pathCodec: PathCodec[P],
      private val queryCodec: QueryCodec[Q],
      private val tc: TConcat.Aux[P, Q, A]
  ) extends UriCodec[A]:
    override def extract( uri: Uri ): Option[A] =
      (
        pathCodec.unapply( uri.path ),
        queryCodec.unapply( uri.query.multiParams )
      ).mapN( tc.concat )

    override def build( a: A ): Uri =
      val ( p, q ) = tc.expand( a )
      Uri( path = pathCodec( p ), query = Query.fromMap( queryCodec( q ) ) )

  def apply[P]( pathCodec: PathCodec[P] ): UriCodec[P] = apply( pathCodec, QueryCodec.Empty )

  def apply[P, Q]( pathCodec: PathCodec[P], queryCodec: QueryCodec[Q] )( using tc: TConcat[P, Q] ): UriCodec[tc.Out] =
    new Default( pathCodec, queryCodec, tc )

  class Constant( private val codec: UriCodec[Unit] ):
    def apply(): Uri = codec.apply( () )

    def unapply( uri: Uri ): Boolean = codec.unapply( uri ).isDefined

  given Conversion[PathCodec[Unit], UriCodec.Constant] = c => new Constant( apply( c ) )
  given Conversion[UriCodec[Unit], UriCodec.Constant]  = c => new Constant( c )

trait UriCodecLowPriority:
  given [A] => Conversion[PathCodec[A], UriCodec[A]] = UriCodec[A]( _ )
