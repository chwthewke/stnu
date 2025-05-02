package net.chwthewke.stnu
package spa.css

case class Classes( classes: Vector[CssClass] ):
  def +( bc: CssClass ): Classes          = copy( classes = classes :+ bc )
  def +:( bc: CssClass ): Classes         = copy( classes = bc +: classes )
  def +( bc: Classes ): Classes           = copy( classes = classes ++ bc.classes )
  def +( bc: Option[CssClass] ): Classes  = copy( classes = classes ++ bc )
  def +:( bc: Option[CssClass] ): Classes = copy( classes = bc ++: classes )

object Classes:
  def apply( classes: CssClass* ): Classes = Classes( classes.toVector )
