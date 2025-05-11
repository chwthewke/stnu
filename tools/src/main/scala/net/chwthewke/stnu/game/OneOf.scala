package net.chwthewke.stnu
package game

import cats.Monoid
import cats.MonoidK
import cats.Show
import cats.Traverse

opaque type OneOf[A] = Vector[A]
object OneOf:
  def one[A]( item: A ): OneOf[A]                       = Vector( item )
  inline def apply[A]( items: Vector[A] ): OneOf[A]     = items
  given [A: Show] => Show[OneOf[A]]                     = Show[Vector[A]]
  given Traverse[OneOf]                                 = Traverse[Vector]
  given oneOfMonoidK: MonoidK[OneOf]                    = MonoidK[Vector]
  given [A] => Monoid[OneOf[A]]                         = oneOfMonoidK.algebra
  extension [A]( oneOf: OneOf[A] ) def items: Vector[A] = oneOf
