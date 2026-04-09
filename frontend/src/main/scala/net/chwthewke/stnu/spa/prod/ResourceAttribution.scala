package net.chwthewke.stnu
package spa
package prod

import cats.Group
import cats.Monoid
import cats.syntax.all.*

import data.Countable
import model.Item
import model.Recipe

private opaque type Items = Vector[Countable[Double, Item]]

private object Items:
  inline def apply( items: Vector[Countable[Double, Item]] ): Items = items

  extension ( self: Items )
    def items: Vector[Countable[Double, Item]] = self
    def plus( other: Items ): Items            = ( self ++ other ).gather
    def times( d: Double ): Items              = self.map( _.mapAmount( d * _ ) )

  given Group[Items]:
    override def inverse( a: Items ): Items           = a.times( -1d )
    override def empty: Items                         = Vector.empty
    override def combine( x: Items, y: Items ): Items = x.plus( y )

object ResourceAttribution:
  def apply(
      env: Env,
      extractedItems: Vector[Countable[Double, Item]],
      processes: Vector[ClockedRecipe]
  ): Map[ClockedRecipe, Double] =
    def extractedItemsMap: Map[ClassName[Item], Double] =
      extractedItems.map( ci => ( ci.item.className, ci.amount ) ).toMap
    def normalize( resources: Vector[Countable[Double, Item]] ): Double =
      resources.foldMap: ci =>
        extractedItemsMap.get( ci.item.className ).foldMap( e => ci.amount / math.sqrt( e ) ) // vibin'
    val normalizedResources: Map[ClockedRecipe, Double] =
      resourceCosts( env, extractedItems, processes )
        .filter {
          case ( cr, _ ) =>
            cr.recipe match
              case _: Recipe.Manufacturing => true
              case _                       => false
        }
        .fmap( normalize )
    normalizedResources.values.maxOption
      .filter( _ > Countable.Tolerance )
      .foldMap( d => normalizedResources.fmap( _ / d ) )

  type Process = ( ClockedRecipe, Int )

  // providers of 1 item
  case class Providers(
      processes: Vector[Countable[Double, ( Process, Double )]], // process, weight of item as product
      extracted: Double
  ):
    val total: Double = extracted + processes.foldMap( _.amount )
    def providing( amount: Double ): ( Vector[Countable[Double, Process]], Double ) =
      val frac: Double = amount / total
      (
        processes.map { case Countable( ( p, w ), _ ) => Countable( p, frac * w ) },
        frac * extracted
      )

    override def toString: String =
      s"""  TOTAL: $total
         |  EXTRACTED: $extracted
         |  ${processes
          .map( cp => s"${cp.item._1._1.recipe.displayNameNoAlt}: ${cp.amount} (w=${cp.item._2})" )
          .mkString( "\n  " )}""".stripMargin

  object Providers:
    given Monoid[Providers]:
      override def empty: Providers = Providers( Vector.empty, 0d )

      override def combine( x: Providers, y: Providers ): Providers =
        Providers( ( x.processes ++ y.processes ).gather, x.extracted + y.extracted )

    def extracted( amount: Double ): Providers                                   = Providers( Vector.empty, amount )
    def processed( amount: Double, process: Process, weight: Double ): Providers =
      Providers( Vector( Countable( ( process, weight ), amount ) ), 0d )

  def providers(
      extractedItems: Vector[Countable[Double, Item]],
      processes: Vector[Process]
  ): Map[Item, Providers] =
    extractedItems.foldMap( ci => Map( ci.item -> Providers.extracted( ci.amount ) ) ) |+|
      processes.foldMap:
        case process @ ( p, _ ) =>
          val ( weightedProducts, totalWeight ) =
            p.productsPerMinute.foldMap: ci =>
              val w: Double = math.pow( 2d, ci.item.tier.value ) // rough correction, once again
              ( Vector( ( ci, w ) ), w )
          weightedProducts.foldMap:
            case ( ci, w ) =>
              Map( ci.item -> Providers.processed( ci.amount, process, w / totalWeight ) )

  def providersOf(
      process: Process,
      providers: Map[Item, Providers]
  ): ( Vector[Countable[Double, Process]], Vector[Countable[Double, Item]] ) =
    val ( allProcesses, allExtracted ) =
      process._1.ingredientsPerMinute
        .foldMap: ci =>
          val ( processes, extracted ) = providers.get( ci.item ).foldMap( _.providing( ci.amount ) )
          ( processes, Vector( Countable( ci.item, extracted ) ).mapFilter( _.significant ) )
    ( allProcesses.gather, allExtracted )

  private def zipWith[A, B, C]( v: Vector[A], w: Vector[B] )( f: ( A, B ) => C ): Vector[C] =
    v.lazyZip( w ).map( f )

  def solveLinear(
      withIndex: Vector[Process],
      eqs: Vector[( Process, Vector[Countable[Double, Process]], Vector[Countable[Double, Item]] )]
  ): Map[ClockedRecipe, Vector[Countable[Double, Item]]] =
    val s                           = eqs.length
    val mat: Vector[Vector[Double]] =
      eqs.map:
        case ( ( p, i ), ps, _ ) =>
          ps.foldLeft( Vector.fill( s )( 0d ).updated( i, 1d ) ):
            case ( acc, Countable( ( _, j ), a ) ) =>
              acc.updated( j, acc( j ) - a )

    def solveLowerTriangular( l: LU.Matrix, b: Vector[Items] ): Vector[Items] =
      // solve L·y = b
      // L_11*y_1 = b_1 => y_1 = b_1 / L_11
      // L_21*y_1 + L_22*y_2 = b_2 => y2 = (b_2 - L_21 * y_1) / L_22
      // etc.
      ( 0 until s ).foldLeft( Vector.empty[Items] ): ( acc, i ) =>
        val r: Vector[Double] = l( i )
        val sum: Items        = zipWith( r, acc )( ( x, y ) => y.times( x ) ).combineAll
        acc :+ b( i ).remove( sum ).times( 1d / r( i ) )

    def solveUpperTriangular( u: LU.Matrix, b: Vector[Items] ): Vector[Items] =
      // solve U·x = y
      // U_ss * x_s = y_s => x_s = y_s / U_ss
      // U_(s-1)(s-1) * x_(s-1) + U(s-1)s * x(s) = y_(s-1) => ... okay we'll cheat
      solveLowerTriangular( u.reverseIterator.map( _.reverse ).toVector, b.reverse ).reverse

    val res = LU( mat )
    // println( s"withIndex=${withIndex.map { case ( p, i ) => s"$i. ${p.recipe.displayNameNoAlt}" }}" )
    // println( s"mat=$mat" )
    // println( s"res=$res" )
    res.foldMap:
      case ( l, u, p ) =>
        val b: Vector[Items] = ( 0 until s ).map( i => Items( eqs( p( i ) )._3 ) ).toVector
        val y: Vector[Items] = solveLowerTriangular( l, b )
        val x: Vector[Items] = solveUpperTriangular( u, y )

        withIndex.foldMap:
          case ( p, i ) =>
            Map( p -> x( i ).items )

  def resourceCosts(
      env: Env,
      extractedItems: Vector[Countable[Double, Item]],
      processes: Vector[ClockedRecipe]
  ): Map[ClockedRecipe, Vector[Countable[Double, Item]]] =
    val withIndex: Vector[Process]         = processes.zipWithIndex
    val allProviders: Map[Item, Providers] = providers( extractedItems, withIndex )
    // println( s"""all providers
    //             |${allProviders.map { case ( i, p ) => s"${i.displayName}\n$p" }.mkString( "\n" )}""".stripMargin )
    val equations: Vector[( Process, Vector[Countable[Double, Process]], Vector[Countable[Double, Item]] )] =
      withIndex.map: p =>
        val ( providers, extracted ) = providersOf( p, allProviders )
        // println(
        //   s"""Providers of ${p._1.recipe.displayNameNoAlt}
        //      |  ${providers.map( cp => s"${cp.item._1.recipe.displayNameNoAlt} x ${cp.amount}" ).mkString( "\n  " )}
        //      |  extracted: ${extracted
        //       .map( ci => s"${ci.item.displayName} x ${ci.amount}" )
        //       .mkString( ", " )}""".stripMargin
        // )
        ( p, providers, extracted )
    solveLinear( withIndex, equations )
