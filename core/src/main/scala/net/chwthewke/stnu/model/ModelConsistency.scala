package net.chwthewke.stnu
package model

import cats.Applicative
import cats.data.ValidatedNel
import cats.syntax.all.*
import mouse.option.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.newts.Max
import data.newts.Min

object ModelConsistency:
  def apply(
      model: Model,
      extractionFilter: ( Item, ExtractorType ) => Boolean = ( _, _ ) => true,
      recipeFilter: Recipe.NonExtraction => Boolean = _ => true
  ): Either[String, Model] =
    retier( feasible( maskExcluded( model ) )( extractionFilter, recipeFilter ) )

  private val excludedProducts: Vector[String] =
    Vector(
      "biofuel",
      "gas nobelisk",
      "biomass",
      "alien protein",
      "alien dna"
    )

  def maskExcluded( model: Model ): Model =
    val excludedItems: SortedSet[ClassName[Item]] =
      model.items
        .filter:
          case ( cn, item ) =>
            excludedProducts.exists( item.displayName.toLowerCase.contains ) &&
            !model.extractedItems.exists( _.className == cn )
        .keySet

    def excludedRecipe( recipe: Recipe ): Boolean =
      recipe.items.exists( item => excludedItems.contains( item.className ) )

    val allowedManufacturingRecipes: Vector[Recipe.Manufacturing] =
      model.manufacturingRecipes.filterNot( excludedRecipe )
    val allowedPowerRecipes: Vector[Recipe.PowerGeneration] =
      model.powerRecipes.filterNot( excludedRecipe )

    model.copy(
      items = model.items.removedAll( excludedItems ),
      manufacturingRecipes = allowedManufacturingRecipes,
      powerRecipes = allowedPowerRecipes
    )

  def feasible( model: Model )(
      extractionFilter: ( Item, ExtractorType ) => Boolean,
      recipeFilter: Recipe.NonExtraction => Boolean
  ): Model =
    val allowedExtractionRecipes: SortedMap[( Item, Machine ), ExtractionRecipes] = model.extractionRecipes.filter:
      case ( ( item, machine ), _ ) => machine.machineType.extractor.exists( extractionFilter( item, _ ) )

    val ( feasibleItems: Set[ClassName[Item]], feasibleRecipes: Set[ClassName[Recipe.NonExtraction]] ) =
      val allowedExtractionItems: Set[ClassName[Item]] = allowedExtractionRecipes.keySet.map( _._1.className )
      Feasible(
        model,
        allowExtractedItem = ( item: Item ) => allowedExtractionItems.contains_( item.className ),
        allowRecipe = recipeFilter
      )

    model.copy(
      items = model.items.filter( item => feasibleItems.contains( item._1 ) ),
      extractedItems = model.extractedItems.filter( item => feasibleItems.contains( item.className ) ),
      extractionRecipes = allowedExtractionRecipes,
      manufacturingRecipes =
        model.manufacturingRecipes.filter( recipe => feasibleRecipes.contains( recipe.className ) ),
      powerRecipes = model.powerRecipes.filter( recipe => feasibleRecipes.contains( recipe.className ) )
    )

  trait RecipeFunctionK[F[_]] {
    def manufacturing( recipe: Recipe.Manufacturing ): F[Recipe.Manufacturing]
    def powerGeneration( recipe: Recipe.PowerGeneration ): F[Recipe.PowerGeneration]
    def extraction( recipe: Recipe.Extraction ): F[Recipe.Extraction]
  }

  private def modifyRecipes[F[_]: Applicative]( f: RecipeFunctionK[F] )( model: Model ): F[Model] =
    val manufacturingRecipes = model.manufacturingRecipes.traverse( f.manufacturing )
    val powerRecipes         = model.powerRecipes.traverse( f.powerGeneration )
    val extractionRecipes    = model.extractionRecipes.traverse:
      case ExtractionRecipes.Fixed( recipe )      => f.extraction( recipe ).map( ExtractionRecipes.Fixed( _ ) )
      case ExtractionRecipes.Variable( byPurity ) =>
        byPurity.traverse( f.extraction ).map( ExtractionRecipes.Variable( _ ) )
    ( manufacturingRecipes, powerRecipes, extractionRecipes ).mapN( ( m, p, e ) =>
      model.copy( manufacturingRecipes = m, powerRecipes = p, extractionRecipes = e )
    )

  private def modifyItems[F[_]: Applicative]( f: Item => F[Item] )( model: Model ): F[Either[String, Model]] =
    model.items.traverse( f ).map( items => model.withItems( items = items ) )

  private def updateItemTier( recipes: SortedMap[ClassName[Recipe], Recipe] )(
      item: Item
  ): ( Vector[ClassName[Any]], Item ) =
    val lowestRecipeTier =
      recipes
        .foldMap: recipe =>
          Option.when( recipe.productsList.exists( _.item.className == item.className ) )( Min( recipe.category.tier ) )
        .map( _.getMin )
    lowestRecipeTier
      .filter( _ > item.tier )
      .fold( ( Vector.empty, item ) )( t => ( Vector( item.className ), item.copy( tier = t ) ) )

  private val updateRecipeTier: RecipeFunctionK[[A] =>> ( Vector[ClassName[Any]], A )] =
    new RecipeFunctionK[[A] =>> ( Vector[ClassName[Any]], A )]:
      private def updatedTier( recipe: Recipe ): Option[Tier] =
        recipe.ingredients.foldMap( ci => Max( ci.item.tier ).some ).map( _.getMax ).filter( _ > recipe.category.tier )

      private def updateRecipe[R <: Recipe](
          recipe: R
      )( updateTier: ( Tier, R ) => R ): ( Vector[ClassName[Any]], R ) =
        updatedTier( recipe ).cata(
          tier => ( Vector( recipe.className ), updateTier( tier, recipe ) ),
          ( Vector.empty, recipe )
        )

      override def manufacturing(
          manufacturing: Recipe.Manufacturing
      ): ( Vector[ClassName[Any]], Recipe.Manufacturing ) =
        updateRecipe( manufacturing )( ( tier, recipe ) =>
          recipe.copy( category = recipe.category match
            case RecipeCategory.Milestone( _ )     => RecipeCategory.Milestone( tier )
            case RecipeCategory.Alternate( _ )     => RecipeCategory.Alternate( tier )
            case RecipeCategory.Mam( _, research ) => RecipeCategory.Mam( tier, research ) )
        )

      override def powerGeneration(
          powerGeneration: Recipe.PowerGeneration
      ): ( Vector[ClassName[Any]], Recipe.PowerGeneration ) =
        updateRecipe( powerGeneration )( ( tier, recipe ) =>
          recipe.copy( category = RecipeCategory.PowerGeneration( tier ) )
        )

      override def extraction( extraction: Recipe.Extraction ): ( Vector[ClassName[Any]], Recipe.Extraction ) =
        updateRecipe( extraction )( ( tier, recipe ) => recipe.copy( category = RecipeCategory.Extraction( tier ) ) )

  def recomputeTiers( model: Model ): Either[String, Model] =
    model.tailRecM: acc =>
      val ( updated, nextE ) =
        for
          updatedRecipes <- modifyRecipes( updateRecipeTier )( acc )
          updatedBoth    <- modifyItems( updateItemTier( acc.recipes ) )( updatedRecipes )
        yield updatedBoth
      nextE.map: model =>
        if ( updated.isEmpty ) Right( model ) else Left( model )

  def retier( model: Model ): Either[String, Model] =
    def extractTier(
        tier: Tier,
        confirmedItems: Map[ClassName[Item], Tier],
        confirmedRecipes: Map[ClassName[Recipe.NonExtraction], Tier]
    ): ( Map[ClassName[Item], Tier], Map[ClassName[Recipe.NonExtraction], Tier] ) =

      val ci =
        model.items.values.toVector
          .filter: item =>
            !confirmedItems.contains( item.className ) && item.tier <= tier
      val cr =
        ( model.manufacturingRecipes ++ model.powerRecipes )
          .filter: recipe =>
            !confirmedRecipes.contains( recipe.className ) && recipe.category.tier <= tier

      val ( fi, fr ) = Feasible(
        model,
        allowOtherItem = ci.toSet,
        allowRecipe = cr.toSet,
        initialItems = confirmedItems.keySet,
        initialRecipes = confirmedRecipes.keySet
      )

      ( fi.map( ( _, tier ) ).toMap ++ confirmedItems, fr.map( ( _, tier ) ).toMap ++ confirmedRecipes )

    @tailrec
    def retierLoop(
        maxTier: Tier,
        tier: Tier,
        confirmedItems: Map[ClassName[Item], Tier],
        confirmedRecipes: Map[ClassName[Recipe.NonExtraction], Tier]
    ): ( Map[ClassName[Item], Tier], Map[ClassName[Recipe.NonExtraction], Tier] ) =
      if ( tier > maxTier ) ( confirmedItems, confirmedRecipes )
      else
        val ( nextConfirmedItems, nextConfirmedRecipes ) =
          extractTier( tier, confirmedItems, confirmedRecipes )
        retierLoop( maxTier, Tier( tier.value + 1 ), nextConfirmedItems, nextConfirmedRecipes )

    def retierManufacturingCategory(
        category: RecipeCategory.Manufacturing,
        newTier: Tier
    ): RecipeCategory.Manufacturing =
      category match
        case RecipeCategory.Milestone( tier )     => RecipeCategory.Milestone( newTier )
        case RecipeCategory.Alternate( tier )     => RecipeCategory.Alternate( newTier )
        case RecipeCategory.Mam( tier, research ) => RecipeCategory.Mam( newTier, research )

    def updateModel(
        retieredItems: Map[ClassName[Item], Tier],
        retieredRecipes: Map[ClassName[Recipe.NonExtraction], Tier]
    ): Either[String, Model] =
      modifyItems( item =>
        retieredItems
          .get( item.className )
          .map( tier => item.copy( tier = tier ) )
          .toValidNel( item.className.show )
      )( model )
        .leftMap( nel => nel.mkString_( "Missed items: ", ", ", "" ) )
        .andThen( _.toValidated )
        .andThen: updatedItems =>
          modifyRecipes(
            new RecipeFunctionK[ValidatedNel[String, *]]:
              override def manufacturing( recipe: Recipe.Manufacturing ): ValidatedNel[String, Recipe.Manufacturing] =
                retieredRecipes
                  .get( recipe.className )
                  .map( tier => recipe.copy( category = retierManufacturingCategory( recipe.category, tier ) ) )
                  .toValidNel( recipe.className.show )

              override def powerGeneration(
                  recipe: Recipe.PowerGeneration
              ): ValidatedNel[String, Recipe.PowerGeneration] =
                retieredRecipes
                  .get( recipe.className )
                  .map( tier => recipe.copy( category = RecipeCategory.PowerGeneration( tier ) ) )
                  .toValidNel( recipe.className.show )

              override def extraction( recipe: Recipe.Extraction ): ValidatedNel[String, Recipe.Extraction] =
                recipe.validNel
          )( updatedItems ).leftMap( nel => nel.mkString_( "Missed recipes: ", ", ", "" ) )
        .toEither

    for
      maxTier <- ( model.items.values.map( _.tier ) ++ model.recipes.values.map( _.category.tier ) ).maxOption
                   .toRight( "empty model" )
      ( retieredItems, retieredRecipes ) =
        retierLoop(
          maxTier,
          Tier( 0 ),
          model.extractedItems.map( item => ( item.className, item.tier ) ).toMap,
          Map.empty
        )
      result <- updateModel( retieredItems, retieredRecipes )
    yield result
