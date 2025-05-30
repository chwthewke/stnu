package net.chwthewke.stnu
package spa
package prod

import cats.Monoid
import cats.syntax.all.*
import scala.collection.immutable.SortedMap

import data.Countable
import model.Item

class ItemIO[+A <: SrcDest](
    val sources: Vector[Countable[Double, A & SrcDest.Src]],
    val destinations: Vector[Countable[Double, A & SrcDest.Dest]]
)

object ItemIO:
  given [A <: SrcDest] => Monoid[ItemIO[A]]:
    override def empty: ItemIO[A] = new ItemIO( Vector.empty, Vector.empty )

    override def combine( x: ItemIO[A], y: ItemIO[A] ): ItemIO[A] =
      new ItemIO(
        ( x.sources ++ y.sources ).gather,
        ( x.destinations ++ y.destinations ).gather
      )

  def from[A <: SrcDest.Src]( src: A, amount: Double ): ItemIO[A] =
    new ItemIO( Countable( src, amount ).significant.toVector, Vector.empty )

  def to[A <: SrcDest.Dest]( dest: A, amount: Double ): ItemIO[A] =
    new ItemIO( Vector.empty, Countable( dest, amount ).significant.toVector )

  def of( recipes: List[ClockedRecipe], requested: Map[Item, Double] ): SortedMap[Item, ItemIO[SrcDest]] =
    val steps: SortedMap[Item, ItemIO[SrcDest]] =
      recipes.foldMap: recipe =>
        val ingredientsIO: SortedMap[Item, ItemIO[SrcDest]] =
          recipe.ingredientsPerMinute.foldMap: ci =>
            SortedMap( ci.item -> to( SrcDest.Step( recipe.recipe.className ), ci.amount ) )
        val productsIO: SortedMap[Item, ItemIO[SrcDest]] =
          recipe.productsPerMinute.foldMap: ci =>
            SortedMap( ci.item -> from( SrcDest.Step( recipe.recipe.className ), ci.amount ) )
        ingredientsIO |+| productsIO
    val externalAmounts: List[Countable[Double, Item]] =
      recipes.foldMap( _.itemsPerMinute ).gather.mapFilter( _.significant )
    val externalIO: SortedMap[Item, ItemIO[SrcDest]] =
      externalAmounts.foldMap: ci =>
        if ( ci.amount < 0 ) SortedMap( ci.item -> from( SrcDest.Input, ci.amount ) )
        else
          val reqAmt: Double               = requested.getOrElse( ci.item, 0d )
          val byProductIO: ItemIO[SrcDest] = to( SrcDest.Byproduct, ci.amount - reqAmt )
          val requestedIO: ItemIO[SrcDest] = to( SrcDest.Requested, reqAmt )
          SortedMap( ci.item -> ( byProductIO |+| requestedIO ) )

    steps |+| externalIO
