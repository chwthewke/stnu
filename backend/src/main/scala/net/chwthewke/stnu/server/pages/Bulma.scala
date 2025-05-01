package net.chwthewke.stnu
package server
package pages

import scalatags.Text.Modifier
import scalatags.text.Builder

opaque type BulmaClass = String

object BulmaClass:
  inline def apply( cls: String ): BulmaClass        = cls
  extension ( self: BulmaClass ) def `class`: String = self

  private class BulmaClassModifier( cls: String ) extends Modifier {
    override def applyTo( t: Builder ): Unit = t.appendAttr( "class", Builder.GenericAttrValueSource( cls ) )
  }

  given Conversion[BulmaClass, Modifier]:
    override def apply( bulmaClass: BulmaClass ): Modifier = new BulmaClassModifier( bulmaClass.`class` )

trait Bulma:
  def cls( name: String ): BulmaClass

  val themeDark: BulmaClass  = cls( "theme-dark" )
  val themeLight: BulmaClass = cls( "theme-light" )

  val title: BulmaClass = cls( "title" )

  val is1: BulmaClass = cls( "is-1" )
  val is2: BulmaClass = cls( "is-2" )
  val is3: BulmaClass = cls( "is-3" )
  val is4: BulmaClass = cls( "is-4" )
  val is5: BulmaClass = cls( "is-5" )
  val is6: BulmaClass = cls( "is-6" )

object Bulma extends Bulma:
  override def cls( name: String ): BulmaClass = BulmaClass( name )

  object Prefixed extends Bulma:
    override def cls( name: String ): BulmaClass = BulmaClass( "bulma-" + name )
