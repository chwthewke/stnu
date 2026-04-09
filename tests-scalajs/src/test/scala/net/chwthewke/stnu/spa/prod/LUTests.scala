package net.chwthewke.stnu
package spa.prod

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*

class LUTests extends ScalaCheckSuite:

  val D: Double = math.log( 1000d )

  def mat(
      sizeGen: Gen[Int] = Gen.choose( 1, 50 ),
      elemGen: Gen[Double] = Gen.choose( -D, D ).map( math.exp )
  ): Gen[LU.Matrix] =
    sizeGen.flatMap: s =>
      Gen.containerOfN[Vector, Vector[Double]]( s, Gen.containerOfN[Vector, Double]( s, elemGen ) )

  def check( a: LU.Matrix ): Unit =
    LU( a ) match {
      case None      => fail( "none" )
      case Some( t ) =>
        val ( l, u, p )   = t
        val s             = a.length
        val lu: LU.Matrix =
          ( 0 until s ).toVector.map: i =>
            ( 0 until s ).toVector.map: j =>
              ( 0 until s ).toVector.foldMap: k =>
                l( i )( k ) * u( k )( j )

        ( 0 until s ).foreach: i =>
          ( 0 until s ).foreach: j =>
            val exp = a( p( i ) )( j )
            val act = lu( i )( j )
            val ok  = ( exp == 0 && act == 0 ) ||
              ( exp - act ).abs / math.max( exp.abs, act.abs ) < 1e-3
            assert( ok, ( i, j, exp, act ) )
    }

  def LUProperty( g: Gen[LU.Matrix] ): Prop =
    forAllNoShrink( g )( m => check( m ) )

  test( "empty matrix" ):
    check( Vector.empty )

  property( "dim-1 matrix" ):
    LUProperty( Gen.choose( 1e-5d, 1e5d ).map( d => Vector( Vector( d ) ) ) )

  property( "2x2 matrices, similar elements" ):
    LUProperty( mat( 2, Gen.choose( 0d, 100d ) ) )

  property( "small matrices, similar elements" ):
    LUProperty( mat( Gen.choose( 2, 10 ), Gen.choose( 0d, 100d ) ) )

  property( "small matrices, similar elements, with permutation" ):
    LUProperty(
      mat( Gen.choose( 2, 10 ), Gen.choose( 0d, 100d ) ).map( m => m.updated( 0, m( 0 ).updated( 0, 0d ) ) )
    )

// NOTE the result can be quite wrong in this case. oh well
  property( "small matrices, dissimilar elements" ):
    LUProperty( mat( sizeGen = Gen.choose( 2, 10 ) ) )

  property( "large matrices, similar elements" ):
    LUProperty( mat( elemGen = Gen.choose( 0d, 100d ) ) )

  property( "large matrices, dissimilar elements" ):
    LUProperty( mat() )
