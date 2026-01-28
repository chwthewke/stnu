package net.chwthewke.stnu
package persistence

import cats.Eq
import cats.data.NonEmptyVector
import fs2.Chunk
import fs2.Pull
import fs2.Stream
import scala.annotation.tailrec
import scala.reflect.ClassTag

object Streams:
  private def groupAdjacentNevWith[F[_], A, B, C: ClassTag, K]( stream: Stream[F, ( A, B, C )], key: ( A, B ) => K )(
      using eqA: Eq[A]
  ): Stream[F, ( K, NonEmptyVector[C] )] =
    def go( current: Option[( A, K, Chunk[C] )], s: Stream[F, ( A, B, C )] ): Pull[F, ( K, NonEmptyVector[C] ), Unit] =
      s.pull.uncons.flatMap:
        case None =>
          current match
            case None                 => Pull.done
            case Some( ( _, k, cs ) ) =>
              Pull.output1( ( k, NonEmptyVector.fromVectorUnsafe( cs.toVector ) ) ) >> Pull.done
        case Some( ( hd, tl ) ) =>
          hd.head match
            case None                => go( current, tl )
            case Some( ( a, b, c ) ) =>
              val ( a1, k, out ) = current.getOrElse( ( a, key( a, b ), Chunk.empty ) )
              doChunk( hd, tl, a1, k, List( out ), None )

    @tailrec
    def doChunk(
        chunk: Chunk[( A, B, C )],
        s: Stream[F, ( A, B, C )],
        a: A,
        k: K,
        out: List[Chunk[C]],
        acc: Option[Chunk[( K, Chunk[C] )]]
    ): Pull[F, ( K, NonEmptyVector[C] ), Unit] =
      chunk.indexWhere( v => eqA.neqv( v._1, a ) ) match
        case None =>
          val newOut: List[Chunk[C]]                        = chunk.map { case ( a, b, c ) => c } :: out
          def next: Pull[F, ( K, NonEmptyVector[C] ), Unit] = go( Some( ( a, k, Chunk.concat( newOut.reverse ) ) ), s )
          acc match
            case Some( kcs ) =>
              Pull.output( kcs.map { case ( k, cs ) => ( k, NonEmptyVector.fromVectorUnsafe( cs.toVector ) ) } ) >> next
            case None => next
        case Some( ix ) =>
          val ( matching: Chunk[( A, B, C )], nonMatching: Chunk[( A, B, C )] ) = chunk.splitAt( ix )
          val newOut: List[Chunk[C]]                                            = matching.map( _._3 ) :: out
          val ( a1: A, b1: B, c1: C )                                           = chunk( ix )
          val k1: K                                                             = key( a1, b1 )
          val newAcc: Chunk[( K, Chunk[C] )]                                    =
            Chunk.concat( acc.toList ::: List( Chunk( ( k, Chunk.concat( newOut.reverse ) ) ) ) )

          doChunk( nonMatching, s, a1, k1, Nil, Some( newAcc ) )

    go( None, stream ).stream

  def groupAdjacentByFirstNev[F[_], A: Eq, B: ClassTag](
      stream: Stream[F, ( A, B )]
  ): Stream[F, ( A, NonEmptyVector[B] )] =
    groupAdjacentNevWith[F, A, Unit, B, A]( stream.map { case ( a, b ) => ( a, (), b ) }, ( a, _ ) => a )

  def groupAdjacentRows[F[_], A: Eq, B, C: ClassTag](
      stream: Stream[F, ( A, B, C )]
  ): Stream[F, ( A, B, NonEmptyVector[C] )] =
    groupAdjacentNevWith[F, A, B, C, ( A, B )]( stream, ( a, b ) => ( a, b ) ).map:
      case ( ( a, b ), cs ) => ( a, b, cs )

extension [F[_], A, B, C]( self: Stream[F, ( A, B, C )] )
  def groupAdjacentRows( using Eq[A], ClassTag[C] ): Stream[F, ( A, B, NonEmptyVector[C] )] =
    Streams.groupAdjacentRows( self )

extension [F[_], A, B]( self: Stream[F, ( A, B )] )
  def groupAdjacentByFirstNev( using Eq[A], ClassTag[B] ): Stream[F, ( A, NonEmptyVector[B] )] =
    Streams.groupAdjacentByFirstNev( self )
