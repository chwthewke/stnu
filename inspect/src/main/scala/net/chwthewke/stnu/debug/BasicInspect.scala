package net.chwthewke.stnu
package debug

import cats.Eval
import cats.Foldable
import cats.data.Chain
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

opaque type BasicInspect[A] = Inspect[A]

object BasicInspect extends BasicInspectInstances0:
  inline def apply[A]( inspect: Inspect[A] ): BasicInspect[A]             = inspect
  extension [A]( basicInspect: BasicInspect[A] ) def instance: Inspect[A] = basicInspect

abstract class BasicInspectInstances0 extends BasicInspectInstances1:

  given BasicInspect[Unit]       = inspectPrimitive( "Unit" )
  given BasicInspect[Boolean]    = inspectPrimitive( "Boolean" )
  given BasicInspect[Byte]       = inspectPrimitive( "Byte" )
  given BasicInspect[Short]      = inspectPrimitive( "Short" )
  given BasicInspect[Int]        = inspectPrimitive( "Int" )
  given BasicInspect[Long]       = inspectPrimitive( "Long" )
  given BasicInspect[Float]      = inspectPrimitive( "Float" )
  given BasicInspect[Double]     = inspectPrimitive( "Double" )
  given BasicInspect[BigInt]     = inspectPrimitive( "BigInt", eager = false )
  given BasicInspect[BigDecimal] = inspectPrimitive( "BigDecimal", eager = false )

  given BasicInspect[Char] =
    inspectPrimitive(
      "Char",
      renderer = Renderer.escaped( start = _.append( "'" ), finish = _.append( "'" ) ).contramap( Iterable( _ ) )
    )

  given BasicInspect[String] = inspectPrimitive(
    "String",
    eager = false,
    renderer = Renderer.escaped[String]( start = _.append( '"' ), finish = _.append( '"' ) )
  )

  given [A: Inspect] => BasicInspect[List[A]]           = inspectFoldable( "List" )
  given [A: Inspect] => BasicInspect[Vector[A]]         = inspectFoldable( "Vector" )
  given [A: Inspect] => BasicInspect[NonEmptyList[A]]   = inspectFoldable( "NonEmptyList" )
  given [A: Inspect] => BasicInspect[NonEmptyVector[A]] = inspectFoldable( "NonEmptyVector" )
  given [A: Inspect] => BasicInspect[LazyList[A]]       = inspectIterableLazy( "LazyList" )

  given [A: Inspect] => BasicInspect[SortedSet[A]]                = inspectFoldable( "SortedSet" )
  given [K: Inspect, V: Inspect] => BasicInspect[SortedMap[K, V]] = inspectMap( "SortedMap", ordered = true )

abstract class BasicInspectInstances1 extends BasicInspectInstances2:
  given [A: Inspect] => BasicInspect[Set[A]]                = inspectIterableEager( "Set", ordered = false )
  given [K: Inspect, V: Inspect] => BasicInspect[Map[K, V]] = inspectMap( "Map" )

abstract class BasicInspectInstances2 extends BasicInspectFunctions:
  given [A: Inspect] => BasicInspect[Iterable[A]] = inspectIterableLazy( "Iterable" )

abstract class BasicInspectFunctions:
  final def from[A]( f: A => Data ): BasicInspect[A] = BasicInspect( ( a: A ) => f( a ) )

  final protected def inspectPrimitive[P](
      tag: String,
      eager: Boolean = true,
      renderer: Renderer[P] = Renderer.fromToString[P]
  ): BasicInspect[P] =
    from: value =>
      Data.Prim(
        tag,
        eager,
        if ( eager ) Eval.now( renderer.render( value ) ) else Eval.later( renderer.render( value ) )
      )

  final protected def inspectFoldable[F[_]: Foldable, A](
      name: String
  )( using inspectA: Inspect[A] ): BasicInspect[F[A]] =
    from: value =>
      Data.Coll(
        name,
        ordered = true,
        Data.Lazy( value.foldMap( a => Chain.one( Eval.later( inspectA( a ) ) ) ).toVector )
      )

  final protected def inspectMap[K, V, M <: Map[K, V]]( tag: String, ordered: Boolean = false )( using
      inspectK: Inspect[K],
      inspectV: Inspect[V]
  ): BasicInspect[M] =
    from: value =>
      Data.Map(
        tag,
        ordered,
        value
          .map:
            case ( k, v ) =>
              Eval.later( ( inspectK( k ), Eval.later( inspectV( v ) ) ) )
          .toVector
      )

  protected def inspectIterableEager[A, T <: Iterable[A]]( tag: String, ordered: Boolean = true )( using
      inspect: Inspect[A]
  ): BasicInspect[T] =
    from: value =>
      Data.Coll( tag, ordered, Data.Lazy( value.map( a => Eval.later( inspect( a ) ) ).toVector ) )

  private def chunk[A, B]( it: Iterator[A], size: Int, f: A => B ): ( Vector[B], Boolean ) =
    val b = Vector.newBuilder[B]

    @tailrec
    def go( acc: b.type, count: Int ): ( Vector[B], Boolean ) =
      if ( count >= size )
        ( b.result(), it.hasNext )
      else if ( it.hasNext )
        go( acc += f( it.next() ), count + 1 )
      else
        ( b.result(), false )

    go( b, 0 )

  private def inspectLazy[A]( coll: Iterable[A], chunkSize: Int )( using inspect: Inspect[A] ): Data.Lazy =
    val it = coll.iterator

    def loop: Data.Lazy =
      val ( head, hasMore ) = chunk( it, chunkSize, a => Eval.later( inspect( a ) ) )
      Data.Lazy( head, Option.when( hasMore )( Eval.later( loop ) ) )

    loop

  final protected def inspectIterableLazy[A, T <: Iterable[A]](
      tag: String,
      ordered: Boolean = true,
      chunkSize: Int = 8
  )( using inspect: Inspect[A] ): BasicInspect[T] =
    from: value =>
      Data.Coll(
        tag,
        ordered,
        inspectLazy( value, chunkSize )
      )
