package net.chwthewke.stnu
package debug

import cats.syntax.all.*
import io.circe.parser
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.cats.implicits.*

import model.Model

object InspectInstancesTest:
  // compile-time only "tests"
  Inspect[Model]
  //

  case class P1( x: Int, y: Int )
  case class P2( p: P1 )
  case class R1( x: Int, r: Option[R1] )

  trait Extract[A] {
    def unapply( data: Data ): Option[A]
  }

  object Prim extends Extract[( String, Boolean, String )] {
    def unapply( data: Data ): Option[( String, Boolean, String )] =
      data match
        case Data.Prim( tag, eager, ev ) => ( tag, eager, ev.value ).some
        case _                           => none
  }

  object Tuple extends Extract[List[Data]] {
    def unapply( data: Data ): Option[List[Data]] =
      data match
        case Data.Tuple( elems ) => elems.map( _.value ).toList.some
        case _                   => none
  }

  object Product extends Extract[( String, List[( String, Data )] )] {
    def unapply( data: Data ): Option[( String, List[( String, Data )] )] =
      data match
        case Data.Prod( tag, members ) => ( tag, members.map { case ( k, v ) => ( k, v.value ) }.toList ).some
        case _                         => none
  }

  object Coproduct extends Extract[( String, Data )] {
    def unapply( data: Data ): Option[( String, Data )] =
      data match
        case Data.Coprod( tag, ev ) => ( tag, ev.value ).some
        case _                      => none
  }

  object Coll {
    object Eager extends Extract[( String, Boolean, List[Data] )] {
      def unapply( data: Data ): Option[( String, Boolean, List[Data] )] =
        data match
          case Data.Coll( tag, ordered, elements ) => ( tag, ordered, elements.forceAll.toList ).some
          case _                                   => none
    }
  }

  object Opaque extends Extract[String] {
    def unapply( data: Data ): Option[String] =
      data match
        case Data.Opaque( desc ) => desc.some
        case _                   => none
  }

  object Each {
    class Matcher[A]( val extract: Extract[A] ) {
      def unapply( dataList: List[Data] ): Option[List[A]] = dataList.traverse( extract.unapply )
    }

    private def apply[A]( extract: Extract[A] ): Matcher[A] = new Matcher[A]( extract )

    val Prim: Matcher[( String, Boolean, String )]           = apply( InspectInstancesTest.Prim )
    val Tuple: Matcher[List[Data]]                           = apply( InspectInstancesTest.Tuple )
    val Product: Matcher[( String, List[( String, Data )] )] = apply( InspectInstancesTest.Product )
    val Coproduct: Matcher[( String, Data )]                 = apply( InspectInstancesTest.Coproduct )

    object Coll {
      val Eager: Matcher[( String, Boolean, List[Data] )] = apply( InspectInstancesTest.Coll.Eager )
    }
  }

class InspectInstancesTest extends ScalaCheckSuite:
  import InspectInstancesTest.*

  def inspectProperties[A]( typeDesc: String, gen: Gen[A] )( assertion: A => PartialFunction[Data, Boolean] )( using
      inspect: Inspect[A]
  ): Unit =
    property( s"$typeDesc can be inspected" ):
      forAll( gen ): a =>
        assert( assertion( a ).applyOrElse( clue( inspect( a ) ), _ => false ) )

  inspectProperties[Int]( "Int", arbitrary[Int] ): n =>
    case Prim( "Int", true, repr ) => repr == n.toString

  inspectProperties[String]( "String (alpha-numeric)", Gen.alphaNumStr ): s =>
    case Prim( "String", false, repr ) => clue( repr ) == s""""$s""""

  inspectProperties[String]( "String (arbitrary)", arbitrary[String] ): s =>
    case Prim( "String", false, repr ) => parser.decode[String]( repr ) == Right( s )

  val extractTuple: PartialFunction[Data, List[Data]] = { case Data.Tuple( elems ) => elems.map( _.value ).toList }

  inspectProperties[( Int, Float )]( "(Int, Float)", ( arbitrary[Int], arbitrary[Float] ).tupled ):
    case ( x, y ) => {
      case Tuple( Prim( _, _, vx ) :: Prim( _, _, vy ) :: Nil ) =>
        vx == x.toString && vy == y.toString
    }

  private val genP1: Gen[P1] = ( arbitrary[Int], arbitrary[Int] ).mapN( P1( _, _ ) )

  inspectProperties[P1]( "P1 - a case class with primitive members", genP1 ):
    case P1( x, y ) => {
      case Product( "P1", ( "x", Prim( _, _, vx ) ) :: ( "y", Prim( _, _, vy ) ) :: Nil ) =>
        vx == x.toString && vy == y.toString
    }

  inspectProperties[P2]( "P2 - a case class with a case class member", genP1.map( P2( _ ) ) ):
    case P2( P1( x, y ) ) => {
      case Product(
            "P2",
            ( "p", Product( "P1", ( "x", Prim( _, _, vx ) ) :: ( "y", Prim( _, _, vy ) ) :: Nil ) ) :: Nil
          ) =>
        vx == x.toString && vy == y.toString
    }

  private val genR1: Gen[R1] =
    ( 0, none[R1] ).tailRecM:
      case ( n, acc ) =>
        def finish: Gen[Either[( Int, Option[R1] ), R1]] = arbitrary[Int].map( x => Right( R1( x, acc ) ) )
        if ( n >= 15 ) finish
        else
          Gen
            .option( arbitrary[Int] )
            .flatMap: ox =>
              ox.fold( finish ): x =>
                Left( ( n + 1, R1( x, acc ).some ) )

  inspectProperties[R1]( "R1 - a recursive case class", genR1 ):
    case R1( x, None ) => {
      case Product(
            "R1",
            ( "x", Prim( "Int", true, vx ) ) :: ( "r", Coproduct( "Option", Product( "None", Nil ) ) ) :: Nil
          ) =>
        vx == x.toString
    }
    case R1( x, Some( R1( y, _ ) ) ) => {
      case Product(
            "R1",
            ( "x", Prim( "Int", true, vx ) ) :: (
              "r",
              Coproduct(
                "Option",
                Product( "Some", ( "value", Product( "R1", ( "x", Prim( "Int", true, vy ) ) :: _ :: Nil ) ) :: Nil )
              )
            ) :: Nil
          ) =>
        vx == x.toString && vy == y.toString
    }

  inspectProperties[Vector[Int]]( "Vector[Int]", Gen.containerOfN[Vector, Int]( 10, arbitrary[Int] ) ): ints =>
    {
      case Coll.Eager( "Vector", true, Each.Prim( items ) ) =>
        ints.length == items.length &&
        ints.zip( items ).forall { case ( s, r ) => r._3 == s.toString }
    }

  inspectProperties[Int => String]( "Int => String", Gen.function1( Gen.alphaNumStr ) ): f =>
    {
      case Opaque( desc ) => clue( desc ) == f.toString
    }
