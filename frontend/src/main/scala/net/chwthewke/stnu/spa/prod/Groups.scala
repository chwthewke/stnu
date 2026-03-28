package net.chwthewke.stnu
package spa
package prod

import cats.kernel.CommutativeMonoid
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import model.prod.Group

enum Groups:
  case Nil
  case SubGroups( children: SortedMap[Int, Groups] ) // TODO actually a NonEmptyMap?

  def closeTo( group: Group ): Groups =
    def loop( prefix: Vector[Int], subGroups: Groups ): Groups =
      val isClose: Boolean =
        group.path.startsWith( prefix ) ||
          prefix.startsWith( group.path ) && prefix.length <= group.path.length + 1

      if ( !isClose ) Nil
      else
        subGroups match
          case Nil                   => Nil
          case SubGroups( children ) =>
            SubGroups(
              children.map:
                case ( i, child ) => ( i, loop( prefix :+ i, child ) )
            )

    loop( Vector.empty, this )

  def width: Int = this match
    case Nil                   => 1
    case SubGroups( children ) => children.foldMap( _.width )

  def widthWithNewSiblings: Int = this match
    case Nil                   => 1
    case SubGroups( children ) =>
      1.to( children.lastKey + 1 ).toVector.foldMap( ix => children.get( ix ).fold( 1 )( _.widthWithNewSiblings ) )

  def depth: Int = this match
    case Nil                   => 1
    case SubGroups( children ) => 1 + children.fmap( _.depth ).maximumOption.getOrElse( 0 )

  def subGroups: Option[SortedMap[Int, Groups]] = this match
    case Nil                   => none
    case SubGroups( children ) => children.some

  def subGroupWidthWithNewChild: Int = this match
    case Nil                   => 1
    case SubGroups( children ) => children.lastKey + 1

  @tailrec
  final def get( path: Vector[Int] ): Option[Groups] =
    path.toNev match
      case None            => this.some
      case Some( pathNev ) =>
        this match
          case Groups.Nil                   => none
          case Groups.SubGroups( children ) =>
            children.get( path.head ) match
              case Some( child ) => child.get( path.tail )
              case None          => none

  @tailrec
  final def hasSlot( group: Group ): Boolean =
    group.path.toNev match
      case None        => true
      case Some( nev ) =>
        subGroups match
          case None             => false
          case Some( children ) =>
            0 < nev.head && nev.head <= children.lastKey &&
            ( children.get( nev.head ) match
              case None                => nev.tail.isEmpty
              case Some( childGroups ) => childGroups.hasSlot( Group( nev.tail ) ) )

object Groups:
  def of( flows: Flows ): Groups =
    flows.endSplits.unorderedFoldMap( _.splits.foldMap( t => Groups.one( t._2 ) ) )

  def one( group: Group ): Groups =
    group.path.toNev match
      case Some( nev ) =>
        SubGroups( SortedMap( nev.head -> one( Group( nev.tail ) ) ) )
      case None =>
        Nil

  def merge( g: Groups, h: Groups ): Groups =
    ( g, h ) match
      case ( SubGroups( cg ), SubGroups( ch ) ) => SubGroups( cg |+| ch )
      case ( Nil, _ )                           => h
      case ( _, Nil )                           => g

  given CommutativeMonoid[Groups]:
    override def empty: Groups = Nil

    override def combine( x: Groups, y: Groups ): Groups = merge( x, y )
