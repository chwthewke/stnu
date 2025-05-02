package net.chwthewke.stnu
package spa.css

import cats.syntax.all.*

trait Phosphor extends PhosphorIcons[Classes]

object Phosphor extends Phosphor:
  override def tcls( tpe: String, name: String ): Classes =
    Classes(
      CssClass( "ph" + ( if ( tpe != "regular" ) "-" + tpe else "" ) ),
      CssClass( "ph-" + name )
    )
