package net.chwthewke.stnu
package protocol
package codec

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.http4s.Uri
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*
import org.scalacheck.Prop.*

class UriCodecTests extends ScalaCheckSuite:

  def decode[A]( codec: UriCodec[A], uri: Uri ): Option[A] =
    uri match
      case codec( a ) => Some( a )
      case _          => None

  def pathCodecProperties[A]( desc: String )( patGen: Gen[UriCodec[A]], gen: Gen[A] ): Unit =
    property( s"encode -> decode round-trips: $desc" ):
      forAll( patGen, gen ): ( pat, a ) =>
        decode( pat, pat( a ) ) == a.some

    property( s"decode -> encode round-trips: $desc" ):
      forAll( patGen, gen ): ( pat, a ) =>
        val uri: Uri = pat( a )

        decode( pat, uri ).map( pat( _ ) ) == uri.some

  private val nonEmptyAlphaString: Gen[String] =
    Gen.sized( n => Gen.choose( 1, n max 1 ).flatMap( Gen.stringOfN( _, Gen.alphaChar ) ) )
  private val nonEmptyAlphaNumString: Gen[String] =
    Gen.sized( n => Gen.choose( 1, n max 1 ).flatMap( Gen.stringOfN( _, Gen.alphaNumChar ) ) )
  private val nonEmptyString: Gen[String] = arbitrary[String].suchThat( _.nonEmpty )

  import PathCodec.Empty
  import SegmentCodec._

  pathCodecProperties( "PathCodec.Root" )( Gen.const( PathCodec.Root ), Gen.const( () ) )

  pathCodecProperties( "PathCodec.Empty" )( Gen.const( Empty ), Gen.const( () ) )

  pathCodecProperties( "Const segment" )( nonEmptyAlphaNumString.map( s => Empty / s ), Gen.const( () ) )

  pathCodecProperties( "String segment" )( Gen.const( Empty / String ), nonEmptyString )

  pathCodecProperties( "Byte segment" )( Gen.const( Empty / Byte ), arbitrary[Byte] )
  pathCodecProperties( "Short segment" )( Gen.const( Empty / Short ), arbitrary[Short] )
  pathCodecProperties( "Int segment" )( Gen.const( Empty / Int ), arbitrary[Int] )
  pathCodecProperties( "Long segment" )( Gen.const( Empty / Long ), arbitrary[Long] )
  pathCodecProperties( "Float segment" )( Gen.const( Empty / Float ), arbitrary[Float] )
  pathCodecProperties( "Double segment" )( Gen.const( Empty / Double ), arbitrary[Double] )
  pathCodecProperties( "Char segment" )( Gen.const( Empty / Char ), arbitrary[Char] )
  pathCodecProperties( "Boolean segment" )( Gen.const( Empty / Boolean ), arbitrary[Boolean] )

  pathCodecProperties( "Composite codec (1-0)" )( Gen.const( Empty / Int / "test" ), arbitrary[Int] )
  pathCodecProperties( "Composite codec (0-1)" )( Gen.const( Empty / "test" / String ), nonEmptyString )

  pathCodecProperties( "Composite codec (1-1)" )(
    Gen.const( Empty / Int / String ),
    ( arbitrary[Int], nonEmptyString ).tupled
  )

  pathCodecProperties( "Composite (2-1)" )(
    Gen.const( Empty / Int / String / Long ),
    ( arbitrary[Int], nonEmptyString, arbitrary[Long] ).tupled
  )

  pathCodecProperties( "Composite (1-2)" )(
    Gen.const( Empty / Int / ( String / Long ) ),
    ( arbitrary[Int], nonEmptyString, arbitrary[Long] ).tupled
  )

  pathCodecProperties( "Optional constant segment" )(
    nonEmptyAlphaNumString.map( s => SegmentCodec.const( s ).optional ),
    Gen.option( Gen.const( () ) )
  )

  pathCodecProperties( "Optional segment" )( Gen.const( String.optional ), Gen.option( nonEmptyString ) )

  pathCodecProperties( "Optional composite" )(
    Gen.const( ( Int / String ).optional ),
    Gen.option( ( arbitrary[Int], nonEmptyString ).tupled )
  )

  pathCodecProperties( "Composite (optional + segment)" )(
    Gen.const( ( Empty / "opt" / String ).optional / Int ),
    ( Gen.option( nonEmptyString ), arbitrary[Int] ).tupled
  )

  pathCodecProperties( "Composite (optional + optional), non ambiguous" )(
    Gen.const( ( Empty / "opt1" / String ).optional / ( Empty / "opt2" / Int ).optional ),
    ( Gen.option( nonEmptyString ), Gen.option( arbitrary[Int] ) ).tupled
  )

  pathCodecProperties( "Composite (segment + optional)" )(
    Gen.const( Empty / String / String.optional ),
    ( nonEmptyString, Gen.option( nonEmptyString ) ).tupled
  )

  case class W( v: String )
  val WS: SegmentCodec[W] = String.imap( W( _ ), _.v )
  val genW: Gen[W]        = nonEmptyString.map( W( _ ) )

  pathCodecProperties( "Composite (mapped segment + optional mapped segment)" )(
    Gen.const( Empty / WS / WS.optional ),
    ( genW, Gen.option( genW ) ).tupled
  )

  pathCodecProperties( "Composite (segment + alternative)" )(
    Gen.const( Empty / String / ( Int || String ) ),
    ( nonEmptyString, Gen.either( arbitrary[Int], nonEmptyAlphaString ) ).tupled
  )
