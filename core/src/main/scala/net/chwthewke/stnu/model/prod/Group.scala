package net.chwthewke.stnu
package model
package prod

import cats.Order
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.derivation.ConfiguredCodec
import scala.annotation.tailrec

case class Group( path: Vector[Int] ) derives Order, ConfiguredCodec:
  override def toString: String = path.toNev.fold( "0" )( _.mkString_( "." ) )

  def ancestors: Set[Group] = path.inits.map( Group( _ ) ).toSet

  def parent: Option[Group] = path.toNev.map( nev => Group( nev.init ) )
  // includes self
  def isDescendantOf( other: Group ): Boolean = path.startsWith( other.path )
  // includes self
  def isAncestorOf( other: Group ): Boolean = other.isDescendantOf( this )

  @tailrec
  final def nearestGroupTo( other: Group ): Group =
    if ( other == this ) other
    else
      other.parent match
        case None           => this.parent.getOrElse( Group.root )
        case Some( parent ) =>
          // resp. descendant or sibling-descendant
          if ( this == parent || this.parent.contains( parent ) ) other
          else nearestGroupTo( parent )

object Group:
  val root: Group       = Group( Vector.empty )
  given Show[Group]     = Show.fromToString
  given Ordering[Group] = Order.catsKernelOrderingForOrder
