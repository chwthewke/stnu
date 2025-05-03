package net.chwthewke.stnu
package model

import cats.Show
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder

import data.ImageName

/**
 * Given a texture name and a version, get the path to the icon
 */
opaque type IconIndex = Map[ClassName[Any], ImageName]

object IconIndex:
  inline def apply( icons: Map[ClassName[Any], ImageName] ): IconIndex = icons
  extension ( iconIndex: IconIndex )
    def getIconPath[A]( className: ClassName[A] ): Option[ImageName] = iconIndex.get( className )
    def icons: Map[ClassName[Any], ImageName]                        = iconIndex

  given iconIndexShow: Show[IconIndex] = Show.show: index =>
    val contents: String =
      index.icons.toVector
        .sortBy:
          case ( n, _ ) => n
        .map:
          case ( n, p ) => show"${n.name.padTo( 40, ' ' )} => $p"
        .mkString( "\n  " )
    s"""IconIndex
       |  $contents
       |""".stripMargin

  given Decoder[IconIndex] = Decoder[Map[ClassName[Any], ImageName]]
  given Encoder[IconIndex] = Encoder[Map[ClassName[Any], ImageName]]
