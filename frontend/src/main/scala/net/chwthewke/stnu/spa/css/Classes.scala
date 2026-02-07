package net.chwthewke.stnu
package spa.css

import cats.Monoid
import cats.derived.*

case class Classes( classes: Vector[CssClass] ) derives Monoid:
  def +( bc: CssClass ): Classes          = copy( classes = classes :+ bc )
  def +:( bc: CssClass ): Classes         = copy( classes = bc +: classes )
  def +( bc: Classes ): Classes           = copy( classes = classes ++ bc.classes )
  def +( bc: Option[CssClass] ): Classes  = copy( classes = classes ++ bc )
  def +:( bc: Option[CssClass] ): Classes = copy( classes = bc ++: classes )

object Classes:
  def apply( classes: CssClass* ): Classes = Classes( classes.toVector )
  given Conversion[Option[CssClass], Classes]:
    override def apply( classOpt: Option[CssClass] ): Classes = Classes( classOpt.toVector )
