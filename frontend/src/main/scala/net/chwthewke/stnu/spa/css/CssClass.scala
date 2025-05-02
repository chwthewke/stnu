package net.chwthewke.stnu
package spa.css

opaque type CssClass = String

object CssClass:
  inline def apply( name: String ): CssClass           = name
  extension ( cssClass: CssClass ) def `class`: String = cssClass
