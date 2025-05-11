package net.chwthewke.stnu
package model

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite
import scala.annotation.tailrec

import ingest.Loader

class ModelConsistencyTests extends CatsEffectSuite:
  DataVersionStorage.cases.foreach: version =>
    val modelAttempt = Loader[IO]( version ).use( _.model ).map( ModelConsistency( _ ) ).unsafeRunSync()
    val modelAttemptWithoutFicsmas =
      modelAttempt.flatMap( ModelConsistency( _, ( _, ex ) => ex != ExtractorType.FicsmasTree ) )

    test( s"Enforcing consistency on model version ${version.docsKey} succeeds" ):
      assert( clue( modelAttempt ).isRight )

    ( 0 to 9 ).foreach: t =>
      test( s"In ${version.docsKey}, tier $t items are feasible with tier $t recipes" ):
        modelAttempt match
          case Left( error )  => fail( s"ModelConsistency failed $error" )
          case Right( model ) => feasibleAtTier( model, Tier( t ) )

    test( s"Enforcing consistency without FICSMAS excludes recipes & items" ):
      ( modelAttempt, clue( modelAttemptWithoutFicsmas ) ).tupled match {
        case Left( err ) => fail( err )
        case Right( ( model, noFicsmas ) ) =>
          assert( clue( model.items.size ) - clue( noFicsmas.items.size ) == 16 )
          assert( clue( model.recipes.size ) - clue( noFicsmas.recipes.size ) == 16 )
      }

    ( 0 to 9 ).foreach: t =>
      test( s"In ${version.docsKey} without FICSMAS, tier $t items are feasible with tier $t recipes" ):
        modelAttemptWithoutFicsmas match
          case Left( error )  => fail( s"ModelConsistency failed $error" )
          case Right( model ) => feasibleAtTier( model, Tier( t ) )

  def feasibleAtTier( model: Model, tier: Tier ): Boolean =
    @tailrec
    def loop( itemsAcc: Set[ClassName[Item]], recipesAcc: Set[ClassName[Recipe]] ): Set[ClassName[Item]] =
      val feasibleRecipes =
        model.recipes.values.filter: recipe =>
          recipe.category.tier <= tier &&
            !recipesAcc.contains( recipe.className ) &&
            recipe.ingredients.map( _.item.className ).forall( itemsAcc.contains )
      val feasibleItems: Set[ClassName[Item]] =
        feasibleRecipes.toVector.foldMap( _.productsList.map( _.item.className ).toSet ) -- itemsAcc
      if ( feasibleRecipes.isEmpty && feasibleItems.isEmpty ) itemsAcc
      else loop( itemsAcc ++ feasibleItems, recipesAcc ++ feasibleRecipes.map( _.className ) )

    val tierItems: Vector[Item] = model.items.values.filter( _.tier <= tier ).toVector

    def containAll( feasibleItems: Set[ClassName[Item]] ): Boolean =
      val itemSet = clue( feasibleItems )
      tierItems.forall( item => itemSet.contains( item.className ) )

    containAll( loop( Set.empty, Set.empty ) )
