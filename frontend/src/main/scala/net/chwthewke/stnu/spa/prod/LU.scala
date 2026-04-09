package net.chwthewke.stnu
package spa
package prod

import cats.syntax.all.*

object LU:
  type Matrix = Vector[Vector[Double]] // square, row-major
  val empty: Matrix = Vector.empty
  type Permutation = Vector[Int]

  /**
   * @param mat
   *   a matrix to decompose into L·U form
   * @return
   *   if successful, `Some((L, U, σ))` s.t. `(L·U)(i)(j) == A(σ(i))(j)`
   */
  def apply( mat: Matrix ): Option[( Matrix, Matrix, Permutation )] =
    val s: Int = mat.length
    // println( s"m: $s x ${mat.map( _.length ).distinct}" )
    if ( mat.exists( _.length != s ) ) None
    else loop( mat )

  def loop( mat: Matrix ): Option[( Matrix, Matrix, Permutation )] =
    if ( mat.isEmpty ) ( empty, empty, Vector.empty ).some
    else
      val rowIndex: Option[Int] = mat.indexWhere( _.head != 0 ).some.filter( _ >= 0 )
      rowIndex match
        case None       => None
        case Some( ix ) =>
          val r              = mat( ix )
          val u_11           = r.head
          val u_12           = r.tail
          val ( a_21, a_22 ) =
            mat.zipWithIndex.collect { case ( r, i ) if i != ix => ( r.head, r.tail ) }.unzip
          val l_21      = a_21.map( _ / u_11 )
          val l_21_u_12 = l_21.map( x => u_12.map( x * _ ) )
          loop(
            a_22
              .zip( l_21_u_12 )
              .map { case ( rx, ry ) => rx.zip( ry ).map { case ( x, y ) => x - y } }
          ) match
            case None                       => None
            case Some( ( l_22, u_22, p2 ) ) =>
              val l: Matrix      = ( 1d +: u_12.as( 0d ) ) +: l_21.zip( l_22 ).map { case ( h, t ) => h +: t }
              val u: Matrix      = ( u_11 +: u_12 ) +: u_22.map( 0d +: _ )
              val p: Permutation = ix +: p2.map( i => if ( i >= ix ) i + 1 else i )
              ( l, u, p ).some
