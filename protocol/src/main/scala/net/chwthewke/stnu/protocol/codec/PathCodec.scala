package net.chwthewke.stnu
package protocol
package codec

import cats.syntax.all.*
import org.http4s.Uri

abstract class PathCodec[A]:
  self =>

  final def unapply( path: Uri.Path ): Option[A] =
    extract( path ).flatMap:
      case ( a, rest ) => Option.when( rest.isEmpty )( a )

  def extract( path: Uri.Path ): Option[( A, Uri.Path )]

  def apply( a: A ): Uri.Path

  protected def relative: PathCodec[A]

  protected def toLazy: PathCodec[A] & PathCodec.Lazy[A] = PathCodec.Lazy( self )

  final def imap[B]( f: A => B, g: B => A ): PathCodec[B] =
    imapFilter( a => f( a ).some, g )

  final def imapFilter[B]( f: A => Option[B], g: B => A ): PathCodec[B] =
    new PathCodec.Transformed[A, B]( self, f, g )

  private def flattenTuple[T, U]( using ev: A =:= ( T, U ), tc: TConcat[T, U] ): PathCodec[tc.Out] =
    imap(
      a =>
        val ( t, u ) = ev( a )
        tc.concat( t, u )
      ,
      tu => ev.flip( tc.expand( tu ) )
    )

  def concat[B]( that: PathCodec[B] ): PathCodec[( A, B )] =
    new PathCodec.Concat( this, that )

  def /( value: String ): PathCodec[A] =
    concat[Unit]( new PathCodec.Segment( SegmentCodec.const( value ) ) ).flattenTuple

  def /[B]( segment: SegmentCodec[B] )( using tc: TConcat[A, B] ): PathCodec[tc.Out] =
    /( PathCodec.Segment( segment ) )

  def /[B]( other: PathCodec[B] )( using tc: TConcat[A, B] ): PathCodec[tc.Out] =
    concat[B]( other ).flattenTuple

  def ||[B]( other: PathCodec[B] ): PathCodec[Either[A, B]] =
    new PathCodec.OrElse( this, other )

  def optional: PathCodec[Option[A]] =
    ||( PathCodec.Empty ).imap( _.left.toOption, _.toLeft( () ) )

  def :?[B]( queryCodec: QueryCodec[B] )( using tc: TConcat[A, B] ): UriCodec[tc.Out] =
    UriCodec( this, queryCodec )

object PathCodec:

  trait Lazy[A]:
    self: PathCodec[A] =>

    def extractWith[B]( next: PathCodec[B], path: Uri.Path ): Option[( ( A, B ), Uri.Path )]

    override def extract( path: Uri.Path ): Option[( A, Uri.Path )] =
      extractWith( Empty, path ).map:
        case ( ( a, _ ), rest ) => ( a, rest )

    override protected def toLazy: PathCodec[A] & Lazy[A] = this

  object Lazy:
    class Backtracking[A]( private val self: PathCodec[A] ) extends PathCodec[A] with Lazy[A]:
      override def apply( a: A ): Uri.Path = self.apply( a )

      override def extractWith[B]( next: PathCodec[B], path: Uri.Path ): Option[( ( A, B ), Uri.Path )] =
        for
          ( a, r ) <- self.extract( path )
          ( b, s ) <- next.extract( r )
        yield ( ( a, b ), s )

      override protected def relative: PathCodec[A] =
        val rel: PathCodec[A] = self.relative
        if ( rel eq self ) this else new Backtracking( rel )

    def apply[A]( codec: PathCodec[A] ): PathCodec[A] & Lazy[A] =
      new Backtracking[A]( codec )

  object Empty extends PathCodec[Unit]:
    override def extract( path: Uri.Path ): Option[( Unit, Uri.Path )] = Some( ( (), path ) )
    override def apply( a: Unit ): Uri.Path                            = Uri.Path.empty

    override protected def relative: PathCodec[Unit] = this

  object Root extends PathCodec[Unit]:
    override def extract( path: Uri.Path ): Option[( Unit, Uri.Path )] =
      Option.when( path.absolute )( ( (), path ) )

    override protected def relative: PathCodec[Unit] = Empty

    override def apply( a: Unit ): Uri.Path = Uri.Path.Root

  final class Segment[A]( private val codec: SegmentCodec[A] ) extends PathCodec[A]:
    override def extract( path: Uri.Path ): Option[( A, Uri.Path )] =
      path.segments match
        case s +: ss => codec.unapply( s ).tupleRight( Uri.Path( ss ) )
        case _       => None

    override def apply( a: A ): Uri.Path = Uri.Path( Vector( codec.apply( a ) ) )

    override protected def relative: PathCodec[A] = this

  final class Transformed[A, B]( private val codec: PathCodec[A], private val f: A => Option[B], private val g: B => A )
      extends PathCodec[B]:

    override def extract( path: Uri.Path ): Option[( B, Uri.Path )] =
      for
        ( a, r ) <- codec.extract( path )
        b        <- f( a )
      yield ( b, r )

    override def apply( b: B ): Uri.Path = codec.apply( g( b ) )

    override protected def relative: Transformed[A, B] =
      val rel: PathCodec[A] = codec.relative
      if ( rel eq codec ) this else new Transformed( codec.relative, f, g )

  final class Concat[A, B]( private val lhs: PathCodec[A], private val rhs: PathCodec[B] ) extends PathCodec[( A, B )]:

    override def extract( path: Uri.Path ): Option[( ( A, B ), Uri.Path )] =
      for
        ( a, r ) <- lhs.extract( path )
        ( b, s ) <- ( if ( r.segments.length == path.segments.length ) rhs else rhs.relative ).extract( r )
      yield ( ( a, b ), s )

    override def apply( t: ( A, B ) ): Uri.Path =
      val ( a, b ) = t
      lhs.apply( a ).concat( rhs.apply( b ) )

    override protected def relative: Concat[A, B] =
      val lhsRel = lhs.relative
      val rhsRel = rhs.relative
      if ( ( lhsRel eq lhs ) && ( rhsRel eq rhs ) ) this
      else new Concat( lhsRel, rhsRel )

  final class OrElse[A, B]( left: PathCodec[A], right: PathCodec[B] )
      extends PathCodec[Either[A, B]]
      with Lazy[Either[A, B]]:
    private val lhs: PathCodec[A] & Lazy[A] = left.toLazy
    private val rhs: PathCodec[B] & Lazy[B] = right.toLazy

    override def extractWith[C]( next: PathCodec[C], path: Uri.Path ): Option[( ( Either[A, B], C ), Uri.Path )] =
      lhs
        .extractWith( next, path )
        .map:
          case ( ( a, c ), r ) => ( ( Left( a ), c ), r )
        .orElse:
          rhs
            .extractWith( next, path )
            .map:
              case ( ( b, c ), r ) => ( ( Right( b ), c ), r )

    override def apply( e: Either[A, B] ): Uri.Path =
      e.fold( lhs.apply, rhs.apply )

    override protected def relative: OrElse[A, B] =
      val lhsRel = lhs.relative
      val rhsRel = rhs.relative
      if ( ( lhsRel eq lhs ) && ( rhsRel eq rhs ) ) this
      else new OrElse( lhsRel, rhsRel )
