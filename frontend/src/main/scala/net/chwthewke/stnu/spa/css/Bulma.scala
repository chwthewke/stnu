package net.chwthewke.stnu
package spa
package css

trait Bulma extends BulmaClasses[CssClass]

object Bulma extends Bulma:
  override def cls( name: String ): CssClass = CssClass( "bulma-" + name )
