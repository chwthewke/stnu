package net.chwthewke.stnu
package spa
package prod

import cats.Semigroup
import cats.Show
import cats.derived.*
import cats.syntax.all.*

import model.Footprint

case class IntegratedFootprint( level: IntegratedFootprint.Level, footprint: Footprint ) derives Show:
  import IntegratedFootprint.Level.*

  def combine( other: IntegratedFootprint ): IntegratedFootprint =
    val combinedLevel: IntegratedFootprint.Level = level.max( other.level )
    IntegratedFootprint( combinedLevel, combinedLevel.combineFootprints( footprint, other.footprint ) )

  def integrate: Option[IntegratedFootprint] =
    level.next.map( nextLevel => copy( level = nextLevel ) )

object IntegratedFootprint:
  def row( footprint: Footprint ): IntegratedFootprint = IntegratedFootprint( Level.Row, footprint )

  extension ( self: Option[IntegratedFootprint] )
    def integrate: Option[IntegratedFootprint] = self.flatMap( _.integrate )

  enum Level:
    case Row
    case Floor
    case Building

    def combineFootprints( x: Footprint, y: Footprint ): Footprint = this match
      case Row      => combineRow( x, y )
      case Floor    => combineFloor( x, y )
      case Building => combineBuilding( x, y )

    def displayName: String = toString.toLowerCase
    def next: Option[Level] = Level.withIndexOption( Level.indexOf( this ) + 1 )

  object Level extends Enum[Level] with CatsEnum[Level] with OrderEnum[Level]

  given Semigroup[IntegratedFootprint]:
    override def combine( x: IntegratedFootprint, y: IntegratedFootprint ): IntegratedFootprint = x.combine( y )

  private def combineRow( x: Footprint, y: Footprint ): Footprint =
    Footprint( x.length.max( y.length ), x.width + y.width )

  private def combineFloor( x: Footprint, y: Footprint ): Footprint =
    Footprint( x.length + y.length + 800, x.width.max( y.width ) )

  private def combineBuilding( x: Footprint, y: Footprint ): Footprint =
    Footprint( x.length.max( y.length ), x.width.max( y.width ) )
