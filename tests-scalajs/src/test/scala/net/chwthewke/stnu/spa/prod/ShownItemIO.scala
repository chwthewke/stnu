package net.chwthewke.stnu
package spa.prod

import cats.Show

import data.Countable

case class ShownItemIO( self: ItemIO[SrcDest] ):
  override def toString: String =
    s"""SOURCES
       |  ${self.sources.map( showIO( "<-", _ ) ).mkString( "\n  " )}
       |DESTINATIONS
       |  ${self.destinations.map( showIO( "->", _ ) ).mkString( "\n  " )}
       |""".stripMargin

  def showIO( direction: String, cs: Countable[Double, SrcDest] ): String =
    s"${cs.amount} $direction ${cs.item.displayName}"

object ShownItemIO:
  given Show[ShownItemIO] = Show.fromToString
