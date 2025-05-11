package net.chwthewke.stnu
package game

import cats.Applicative
import cats.Eval
import cats.Traverse
import cats.TraverseFilter
import cats.derived.strict.*
import cats.syntax.all.*
import mouse.option.*
import scala.collection.immutable.SortedMap

import model.RecipeCategory

case class Analyses[A](
    schematics: SortedMap[ClassName[Schematic], A],
    items: SortedMap[ClassName[GameItem], A],
    recipes: SortedMap[ClassName[GameRecipe], A],
    manufacturers: SortedMap[ClassName[Manufacturer], A],
    powerGenerators: SortedMap[ClassName[PowerGenerator], A],
    extractors: SortedMap[ClassName[Extractor], A],
    recipeCategories: Map[ClassName[GameRecipe], RecipeCategory]
):
  def get( item: AnalysisItem ): Option[A] =
    item match
      case AnalysisItem.OfItem( item )           => items.get( item.className )
      case AnalysisItem.OfRecipe( recipe )       => recipes.get( recipe.className )
      case AnalysisItem.OfSchematic( schematic ) => schematics.get( schematic.className )

  def keys: Vector[ClassName[Any]] =
    ( schematics.keys ++ items.keys ++ recipes.keys ++ manufacturers.keys ++ powerGenerators.keys ++ extractors.keys ).toVector

  def merged: Vector[( ClassName[Any], A )] =
    schematics.toVector ++ items ++ recipes ++ powerGenerators ++ extractors

object Analyses:

  given Traverse[Analyses]:
    override def traverse[G[_]: Applicative, A, B]( fa: Analyses[A] )( f: A => G[B] ): G[Analyses[B]] =
      (
        fa.schematics.traverse( f ),
        fa.items.traverse( f ),
        fa.recipes.traverse( f ),
        fa.manufacturers.traverse( f ),
        fa.powerGenerators.traverse( f ),
        fa.extractors.traverse( f )
      ).mapN( Analyses( _, _, _, _, _, _, fa.recipeCategories ) )

    private def allAnalyses[A]( fa: Analyses[A] ): Iterable[A] =
      fa.schematics.values ++ fa.items.values ++ fa.recipes.values
        ++ fa.manufacturers.values ++ fa.powerGenerators.values ++ fa.extractors.values

    override def foldLeft[A, B]( fa: Analyses[A], b: B )( f: ( B, A ) => B ): B =
      allAnalyses( fa ).foldLeft( b )( f )

    override def foldRight[A, B]( fa: Analyses[A], lb: Eval[B] )( f: ( A, Eval[B] ) => Eval[B] ): Eval[B] =
      allAnalyses( fa ).foldRight( lb )( f )

  given TraverseFilter[Analyses]:
    override def traverse: Traverse[Analyses] = Traverse[Analyses]

    override def traverseFilter[G[_], A, B](
        fa: Analyses[A]
    )( f: A => G[Option[B]] )( implicit G: Applicative[G] ): G[Analyses[B]] =
      (
        fa.schematics.traverseFilter( f ),
        fa.items.traverseFilter( f ),
        fa.recipes.traverseFilter( f ),
        fa.manufacturers.traverseFilter( f ),
        fa.powerGenerators.traverseFilter( f ),
        fa.extractors.traverseFilter( f )
      ).mapN( Analyses( _, _, _, _, _, _, fa.recipeCategories ) )

  extension ( data: SchematicsGameData )

    private def findBaseRecipeDependency( altRecipeSchematic: Schematic ): AllOf[AnalysisItem] =
      data.alternateUnlocks
        .get( altRecipeSchematic.className )
        .foldMap( altRecipe => findItemDependency( altRecipe.products.head.item ) )

    private def findItemDependency( itemClass: ClassName[GameItem] ): AllOf[AnalysisItem] =
      data.items
        .get( itemClass )
        .map( item => AllOf.one( AnalysisItem.OfItem( item ) ) )
        .orEmpty

    def analyzeSchematic( schematic: Schematic ): Option[Either[Milestone, AllOf[AnalysisItem]]] =
      schematic.`type` match
        case SchematicType.HardDrive | SchematicType.Shop => None
        case SchematicType.Custom | SchematicType.Customization | SchematicType.Milestone | SchematicType.Tutorial =>
          Some( Left( Milestone( schematic ) ) )
        case SchematicType.Alternate =>
          Some(
            Right(
              data.schematicDependencies
                .get( schematic.className )
                .orEmpty
                .map( AnalysisItem.OfSchematic.apply )
                .widen[AnalysisItem]
                <+>
                  findBaseRecipeDependency( schematic )
            )
          )
        case SchematicType.Mam =>
          Some( Right( schematic.cost.foldMap( c => findItemDependency( c.item ) ) ) )

    def analyzeSchematics: SortedMap[ClassName[Schematic], Either[Milestone, AllOf[AnalysisItem]]] =
      data.schematics
        .mapFilter( schematic => data.analyzeSchematic( schematic ).tupleLeft( schematic.className ) )
        .to( SortedMap )

    private def powerGeneratorByproducts: Vector[( ClassName[GameItem], AllOf[AnalysisItem] )] =
      data.powerGenerators.values.toVector.foldMap: generator =>
        generator.fuels.mapFilter: fuel =>
          fuel.byproduct.map: byproduct =>
            (
              byproduct.item,
              AllOf.allOf(
                Vector(
                  data.items.get( fuel.fuel ).map( AnalysisItem.OfItem( _ ) ),
                  data.powerGeneratorSchematics.get( generator.className ).map( AnalysisItem.OfSchematic( _ ) )
                ).flattenOption
              )
            )

    private def extractionProducts: Vector[( ClassName[GameItem], Either[Milestone, AllOf[AnalysisItem]] )] =
      val solidResources: Vector[GameItem] =
        data.items.values
          .filter: item =>
            item.nativeClass == NativeClass.resourceDescClass && item.form == GameForm.Solid
          .toVector
      val otherResources: Map[ClassName[GameItem], AllOf[AnalysisItem]] =
        data.extractors.values.toVector
          .foldMap: extractor =>
            extractor.allowedResources
              .cata( _.toList, Nil )
              .toVector
              .tupleRight(
                data.extractorSchematics
                  .get( extractor.className )
                  .map( AnalysisItem.OfSchematic( _ ) )
                  .toVector
              )
              .toMap
          .fmap( AllOf.anyOf )

      solidResources.map( item => ( item.className, Left( Milestone.Zero ) ) ).distinct ++
        otherResources.map:
          case ( it, deps ) => ( it, Right( deps ) )

    def analyzeItems: SortedMap[ClassName[GameItem], Either[Milestone, AllOf[AnalysisItem]]] =
      val fromRecipe: SortedMap[ClassName[GameItem], AllOf[AnalysisItem]] =
        data.recipes
          .foldMap: recipe =>
            recipe.products.map( _.item ).foldMap( it => SortedMap( it -> Vector( AnalysisItem.OfRecipe( recipe ) ) ) )
          .fmap( AllOf.anyOf )

      val fromRecipeOrExtracted: SortedMap[ClassName[GameItem], Either[Milestone, AllOf[AnalysisItem]]] =
        ( fromRecipe ++ powerGeneratorByproducts ).fmap( Right( _ ) ) ++ extractionProducts

      fromRecipeOrExtracted ++
        data.items.mapFilter: item =>
          Option.when( !fromRecipeOrExtracted.contains( item.className ) )( Left( Milestone.Zero ) )

    def analyzeRecipe( recipe: GameRecipe ): Either[Milestone, AllOf[AnalysisItem]] =
      Right(
        AllOf.allOf(
          Vector(
            recipe.producedIn.collectFirstSome( data.manufacturerSchematics.get ),
            data.recipeSchematics.get( recipe.className )
          ).flattenOption
            .map( s => AnalysisItem.OfSchematic( s ) ) ++
            recipe.ingredients.mapFilter( ci => data.items.get( ci.item ).map( AnalysisItem.OfItem( _ ) ) )
        )
      )

    def analyzeRecipes: SortedMap[ClassName[GameRecipe], Either[Milestone, AllOf[AnalysisItem]]] =
      data.recipes.map( recipe => ( recipe.className, data.analyzeRecipe( recipe ) ) ).to( SortedMap )

    def analyzeMachines[A](
        machines: Map[ClassName[A], Schematic]
    ): SortedMap[ClassName[A], Either[Milestone, AllOf[AnalysisItem]]] =
      machines.fmap( schem => Right( AllOf.one( AnalysisItem.OfSchematic( schem ) ) ) ).to( SortedMap )

    def analyzeManufacturers: SortedMap[ClassName[Manufacturer], Either[Milestone, AllOf[AnalysisItem]]] =
      analyzeMachines( data.manufacturerSchematics )

    def analyzeGenerators: SortedMap[ClassName[PowerGenerator], Either[Milestone, AllOf[AnalysisItem]]] =
      analyzeMachines( data.powerGeneratorSchematics )

    def analyzeExtractors: SortedMap[ClassName[Extractor], Either[Milestone, AllOf[AnalysisItem]]] =
      analyzeMachines( data.extractorSchematics )

  def init( data: SchematicsGameData ): Analyses[Either[Milestone, AllOf[AnalysisItem]]] =
    Analyses(
      data.analyzeSchematics,
      data.analyzeItems,
      data.analyzeRecipes,
      data.analyzeManufacturers,
      data.analyzeGenerators,
      data.analyzeExtractors,
      Map.empty
    )
