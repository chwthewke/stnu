package net.chwthewke.stnu
package spa

import cats.Traverse
import cats.syntax.all.*
import tyrian.Attr
import tyrian.Elem
import tyrian.Empty
import tyrian.EmptyAttribute
import tyrian.Html

import data.Countable
import model.Item
import model.Recipe
import spa.css.Classes
import spa.css.CssClass

package object views:
  val nbsp: Elem[Nothing] = Html.raw( "span" )( "&nbsp;" )

  def classes( classes: CssClass* ): Attr[Nothing] =
    Html.className := classes.distinct.mkString( " " )

  val icon: Icons.Icon[Nothing] = Icons.icon

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

  given convertAttrOption[A]: Conversion[Option[Attr[A]], Attr[A]] = _.fold[Attr[A]]( EmptyAttribute )( identity )
  given convertElemOption[A]: Conversion[Option[Elem[A]], Elem[A]] = _.fold[Elem[A]]( Empty )( identity )

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
