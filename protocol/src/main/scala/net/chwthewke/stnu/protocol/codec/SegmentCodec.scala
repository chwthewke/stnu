package net.chwthewke.stnu
package protocol
package codec

import cats.syntax.all.*
import org.http4s.Uri

trait SegmentCodec[A]:
  self =>
  def unapply( segment: Uri.Path.Segment ): Option[A]
  def apply( a: A ): Uri.Path.Segment

  def imap[B]( f: A => B, g: B => A ): SegmentCodec[B] =
    new SegmentCodec[B]:
      override def unapply( segment: Uri.Path.Segment ): Option[B] = self.unapply( segment ).map( f )
      override def apply( b: B ): Uri.Path.Segment                 = self.apply( g( b ) )

  def imapFilter[B]( f: A => Option[B], g: B => A ): SegmentCodec[B] =
    new SegmentCodec[B]:
      override def unapply( segment: Uri.Path.Segment ): Option[B] = self.unapply( segment ).flatMap( f )
      override def apply( b: B ): Uri.Path.Segment                 = self.apply( g( b ) )

object SegmentCodec:

  def instance[A]( f: Uri.Path.Segment => Option[A], g: A => Uri.Path.Segment ): SegmentCodec[A] =
    new SegmentCodec[A]:
      override def unapply( segment: Uri.Path.Segment ): Option[A] = f( segment )
      override def apply( a: A ): Uri.Path.Segment                 = g( a )

  val String: SegmentCodec[String]   = SegmentCodec.instance( _.decoded().some, Uri.Path.Segment.encoded )
  val Byte: SegmentCodec[Byte]       = String.imapFilter( _.toByteOption, _.toString )
  val Short: SegmentCodec[Short]     = String.imapFilter( _.toShortOption, _.toString )
  val Int: SegmentCodec[Int]         = String.imapFilter( _.toIntOption, _.toString )
  val Long: SegmentCodec[Long]       = String.imapFilter( _.toLongOption, _.toString )
  val Float: SegmentCodec[Float]     = String.imapFilter( _.toFloatOption, _.toString )
  val Double: SegmentCodec[Double]   = String.imapFilter( _.toDoubleOption, _.toString )
  val Boolean: SegmentCodec[Boolean] = String.imapFilter( _.toBooleanOption, _.toString )
  val Char: SegmentCodec[Char]       = String.imapFilter( s => Option.when( s.length == 1 )( s.head ), _.toString )

  def const( value: String ): SegmentCodec[Unit] =
    String.imapFilter( str => Option.when( str == value )( () ), _ => value )

  def enumSegment[E]( E: Enum[E] ): SegmentCodec[E] = String.imapFilter( E.withNameOption, E.keyOf )

  given [A] => Conversion[SegmentCodec[A], PathCodec[A]] = new PathCodec.Segment( _ )
