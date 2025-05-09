package net.chwthewke.stnu
package model

import algebra.lattice.MeetSemilattice
import cats.Eq
import cats.Monoid
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

case class ResourceDistrib( impureNodes: Int, normalNodes: Int, pureNodes: Int )
    derives Eq,
      Monoid,
      ConfiguredDecoder,
      ConfiguredEncoder:

  def get( purity: ResourcePurity ): Int = purity match
    case ResourcePurity.Impure => impureNodes
    case ResourcePurity.Normal => normalNodes
    case ResourcePurity.Pure   => pureNodes

  def set( purity: ResourcePurity, value: Int ): ResourceDistrib = purity match
    case ResourcePurity.Impure => copy( impureNodes = value )
    case ResourcePurity.Normal => copy( normalNodes = value )
    case ResourcePurity.Pure   => copy( pureNodes = value )

  override def toString: String = show"Impure: $impureNodes, Normal: $normalNodes, Pure: $pureNodes"

object ResourceDistrib:
  def of( purity: ResourcePurity, amount: Int ): ResourceDistrib =
    purity match
      case ResourcePurity.Pure   => ResourceDistrib( 0, 0, amount )
      case ResourcePurity.Normal => ResourceDistrib( 0, amount, 0 )
      case ResourcePurity.Impure => ResourceDistrib( amount, 0, 0 )

  given Show[ResourceDistrib] = Show.fromToString[ResourceDistrib]

  given MeetSemilattice[ResourceDistrib]:
    override def meet( lhs: ResourceDistrib, rhs: ResourceDistrib ): ResourceDistrib =
      ResourceDistrib(
        lhs.impureNodes.min( rhs.impureNodes ),
        lhs.normalNodes.min( rhs.normalNodes ),
        lhs.pureNodes.min( rhs.pureNodes )
      )
