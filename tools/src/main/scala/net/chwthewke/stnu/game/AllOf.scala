package net.chwthewke.stnu
package game

import cats.Monoid
import cats.MonoidK
import cats.Show
import cats.Traverse
import cats.data.Nested
import cats.derived.semiauto
import cats.syntax.all.*

opaque type AllOf[A] = Nested[Vector, OneOf, A]
object AllOf:
  def one[A]( item: A ): AllOf[A]                              = allOf( Vector( item ) )
  def allOf[A]( items: Vector[A] ): AllOf[A]                   = items.map( OneOf.one ).nested
  def anyOf[A]( items: Vector[A] ): AllOf[A]                   = Vector( OneOf( items ) ).nested
  inline def apply[A]( items: Vector[OneOf[A]] ): AllOf[A]     = items.nested
  given [A: Show] => Show[AllOf[A]]                            = semiauto.show[AllOf[A]]
  given Traverse[AllOf]                                        = Traverse[Nested[Vector, OneOf, *]]
  given allOfMonoidK: MonoidK[AllOf]                           = MonoidK[Nested[Vector, OneOf, *]]
  given [A] => Monoid[AllOf[A]]                                = allOfMonoidK.algebra
  extension [A]( allOf: AllOf[A] ) def items: Vector[OneOf[A]] = allOf.value
