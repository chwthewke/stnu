package net.chwthewke.stnu
package game

import cats.Show
import cats.derived.strict.*

case class IconData( dir: String, packageName: String, textureName: String ) derives Show:
  def fullName: String = s"$dir/$textureName"
