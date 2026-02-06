package net.chwthewke.stnu
package spa

import cats.Traverse
import cats.syntax.all.*
import org.scalajs.dom.ModifierKeyEvent
import tyrian.Attr
import tyrian.Elem
import tyrian.Empty
import tyrian.EmptyAttribute
import tyrian.Event
import tyrian.Html

import data.Countable
import model.Item
import model.Recipe
import spa.css.Classes
import spa.css.CssClass

package object views:
  // elements
  val nbsp: Elem[Nothing] = Html.raw( "span" )( "&nbsp;" )

  val icon: Icons.Icon[Nothing] = Icons.icon

  // events

  opaque type KeyModifier = Byte

  object KeyModifier:
    private val META: Byte  = 1
    private val CTRL: Byte  = 2
    private val SHIFT: Byte = 4
    private val ALT: Byte   = 8

    def apply( modifierKeys: ModifierKeyEvent ): KeyModifier =
      ( ( if ( modifierKeys.metaKey ) META else 0 ) |
        ( if ( modifierKeys.ctrlKey ) CTRL else 0 ) |
        ( if ( modifierKeys.shiftKey ) SHIFT else 0 ) |
        ( if ( modifierKeys.altKey ) ALT else 0 ) ).toByte

    extension ( self: KeyModifier )
      def meta: Boolean  = ( self & META ) != 0
      def ctrl: Boolean  = ( self & CTRL ) != 0
      def shift: Boolean = ( self & SHIFT ) != 0
      def alt: Boolean   = ( self & ALT ) != 0

  extension ( self: Html.type )
    def onClickModified[M]( msg: KeyModifier => M ): Attr[M] =
      Event( "click", e => msg( KeyModifier( e.asInstanceOf[ModifierKeyEvent] ) ) )

  // classes utility
  def classes( classes: CssClass* ): Attr[Nothing] =
    Html.className := classes.distinct.mkString( " " )
  extension ( bc: CssClass )
    def +( c: CssClass ): Classes         = Classes( Vector( bc, c ) )
    def +( c: Option[CssClass] ): Classes = Classes( Vector( bc ) ++ c )

  extension ( bc: Option[CssClass] )
    def +( c: CssClass ): Classes         = Classes( bc ++: Vector( c ) )
    def +( c: Option[CssClass] ): Classes = Classes( bc.toVector ++ c )

  given Conversion[CssClass, Classes]               = c => Classes( c )
  given Conversion[Option[CssClass], Attr[Nothing]] = c => c.fold( EmptyAttribute )( c => classes( c ) )
  given Conversion[CssClass, Attr[Nothing]]         = c => classes( c )
  given Conversion[Classes, Attr[Nothing]]          = c => classes( c.classes* )

  // Option conversions
  given convertAttrOption[A]: Conversion[Option[Attr[A]], Attr[A]] = _.fold[Attr[A]]( EmptyAttribute )( identity )
  given convertElemOption[A]: Conversion[Option[Elem[A]], Elem[A]] = _.fold[Elem[A]]( Empty )( identity )

  // yeah, so this is here
  extension ( recipe: Recipe )
    def describe: String =
      def showAmount( d: Double ): String =
        if ( d.isValidInt ) f"${d.toInt}%d"
        else if ( d > 1 ) f"$d%.1f"
        else f"$d%.2f"

      def showItem( item: Countable[Double, Item], perMinute: Countable[Double, Item] ) =
        show"${showAmount( item.amount )} x ${item.item.displayName} @ ${showAmount( perMinute.amount )}/min."

      def showItemList[F[_]: Traverse]( items: F[( Countable[Double, Item], Countable[Double, Item] )] ) =
        items
          .map( showItem.tupled )
          .mkString_( ", " )

      val ingredients = showItemList( recipe.ingredients zip recipe.ingredientsPerMinute )
      val products    = recipe match
        case p: Recipe.Manufacturing   => showItemList( p.products zip p.productsPerMinute )
        case p: Recipe.PowerGeneration => showItemList( p.products zip p.productsPerMinute )
        case p: Recipe.Extraction      => showItemList( ( p.products, p.productsPerMinute ) :: Nil )

      show"$ingredients \u21d2 $products"
