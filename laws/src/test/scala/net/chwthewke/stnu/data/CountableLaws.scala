package net.chwthewke.stnu
package data

import cats.Eq
import cats.data.Const
import cats.data.ZipLazyList
import cats.derived.semiauto
import cats.laws.discipline.BitraverseTests
import cats.laws.discipline.MonadTests
import cats.laws.discipline.TraverseTests
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.cats.implicits.*

class CountableLaws extends DisciplineSuite:

  given [N, A] => ( arbN: Arbitrary[N], arbA: Arbitrary[A] ) => Arbitrary[Countable[N, A]] =
    Arbitrary( ( arbA.arbitrary, arbN.arbitrary ).mapN( Countable( _, _ ) ) )

  given [N: Eq, A: Eq] => Eq[Countable[N, A]] = semiauto.eq[Countable[N, A]]

  given [A] => ( arbA: Arbitrary[LazyList[A]] ) => Arbitrary[ZipLazyList[A]] =
    Arbitrary( arbA.arbitrary.map( ZipLazyList( _ ) ) )

  given [A, B] => ( arbA: Arbitrary[A] ) => Arbitrary[Const[A, B]] =
    Arbitrary( arbA.arbitrary.map( Const( _ ) ) )

  checkAll( "Countable.MonadLaws", MonadTests[Countable[Int, *]].monad[Int, String, Char] )

  checkAll(
    "Countable.TraverseLaws",
    TraverseTests[Countable[Int, *]].traverse[Int, String, Char, Int, ZipLazyList, Const[Int, *]]
  )

  checkAll(
    "Countable.BitraverseLaws",
    BitraverseTests[Countable].bitraverse[ZipLazyList, Int, String, Int, Char, Float, Boolean]
  )
