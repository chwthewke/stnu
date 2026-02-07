package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.Html

import spa.css.Bulma

// NOTE i'm keeping this around just in case
object Grid:
  val b: Bulma = Bulma

  def grid[A]( columns: Int, elements: Vector[( Int, Int, Html[A] )] ): Html[A] =
    Html.div( b.fixedGrid + b.cls( s"has-$columns-cols" ) )(
      Html.div( b.grid + b.`isGap0.5` )(
        elements
          .groupByNev( _._1 )
          .foldMap: row =>
            val rightCells: Int = columns - row.last._2
            row
              .sortBy( _._2 )
              .foldMap:
                case ( _, col, elt ) =>
                  List( Html.div( b.cell + b.cls( s"is-col-start-$col" ) )( elt ) )
            ++ Option.when( rightCells > 0 )( Html.div( b.cell + b.cls( s"is-col-span-$rightCells" ) )() )
      )
    )
