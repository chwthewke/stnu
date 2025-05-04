package net.chwthewke.stnu
package game

import cats.Monoid
import cats.MonoidK
import cats.Order
import cats.Show
import cats.Traverse
import cats.data.Nested
import cats.data.NonEmptyVector
import cats.derived.strict.*
import cats.syntax.all.*
import mouse.option.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import model.RecipeCategory
import model.ResearchCategory
import model.Tier

case class RecipeClassifier( data: GameData ):

  def classifyRecipes: RecipeClassifier.Result = MilestoneAnalyzer.init.run

  import RecipeClassifier.*

  class MilestoneAnalyzer(
      val manufacturingRecipes: Vector[GameRecipe],
      val recipeSchematics: Map[ClassName[GameRecipe], Schematic],
      val schematicDependencies: Map[ClassName[Schematic], AllOf[Schematic]],
      val alternateUnlocks: Map[ClassName[Schematic], GameRecipe],
      val baseRecipes: Map[ClassName[GameItem], GameRecipe],
      val manufacturerSchematics: Map[ClassName[Manufacturer], Schematic],
      val powerGeneratorSchematics: Map[ClassName[PowerGenerator], Schematic],
      val extractorSchematics: Map[ClassName[Extractor], Schematic]
  ):

    private def findBaseRecipe( altRecipeSchematic: Schematic ): AllOf[AnalysisItem] =
      alternateUnlocks
        .get( altRecipeSchematic.className )
        .foldMap( altRecipe => findItemBaseRecipe( altRecipe.products.head.item ) )

    def findItemBaseRecipe( itemClass: ClassName[GameItem] ): AllOf[AnalysisItem] =
      baseRecipes
        .get( itemClass )
        .map( r => AllOf.one( AnalysisItem.OfRecipe( r ) ) )
        .orEmpty

    def analyzeSchematic( schematic: Schematic ): Option[Either[Milestone, AllOf[AnalysisItem]]] =
      schematic.`type` match
        case SchematicType.HardDrive | SchematicType.Shop => None
        case SchematicType.Custom | SchematicType.Customization | SchematicType.Milestone | SchematicType.Tutorial =>
          Some( Left( Milestone( schematic ) ) )
        case SchematicType.Alternate =>
          Some(
            Right(
              schematicDependencies
                .get( schematic.className )
                .orEmpty
                .map( AnalysisItem.OfSchematic.apply )
                .widen[AnalysisItem]
                <+>
                  findBaseRecipe( schematic )
            )
          )
        case SchematicType.Mam =>
          Some( Right( schematic.cost.foldMap( c => findItemBaseRecipe( c.item ) ) ) )

    def analyzeRecipe( recipe: GameRecipe ): Either[Milestone, AllOf[AnalysisItem]] =
      Right(
        AllOf.allOf(
          Vector(
            recipe.producedIn.collectFirstSome( manufacturerSchematics.get ),
            recipeSchematics.get( recipe.className )
          ).flattenOption
            .map( s => AnalysisItem.OfSchematic( s ) )
        )
      )

    def analyzeGenerators: SortedMap[ClassName[PowerGenerator], Either[Milestone, AllOf[AnalysisItem]]] =
      powerGeneratorSchematics
        .fmap( schem => Right( AllOf.one( AnalysisItem.OfSchematic( schem ) ) ) )
        .to( SortedMap )

    def analyzeExtractors: SortedMap[ClassName[Extractor], Right[Nothing, AllOf[AnalysisItem]]] =
      extractorSchematics
        .fmap( schem => Right( AllOf.one( AnalysisItem.OfSchematic( schem ) ) ) )
        .to( SortedMap )

    val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean]:
      override def empty: Boolean                             = false
      override def combine( x: Boolean, y: Boolean ): Boolean = x || y

    private def finishAnalysis[A](
        results: Map[ClassName[A], Either[Milestone, AllOf[AnalysisItem]]]
    ): ( Vector[ClassName[Any]], Map[ClassName[A], Milestone] ) =
      results.toVector
        .traverse:
          case ( cn, e ) =>
            (
              e.as( cn ).toOption.toVector,
              Map( cn -> e.swap.getOrElse( Milestone.Zero ) )
            )
        .fmap( _.foldLeft( Map.empty[ClassName[A], Milestone] )( _ ++ _ ) )

    @tailrec
    private def loop(
        currentAnalyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]]
    ): (
        Map[ClassName[GameRecipe], Milestone],
        Map[ClassName[Extractor], Milestone],
        Map[ClassName[PowerGenerator], Milestone]
    ) =

      val ( progressed, newAnalyses ) =
        given Monoid[Boolean] = boolOrMonoid
        currentAnalyses.traverse: toAnalyze =>
          toAnalyze
            .flatTraverse: reqs =>
              reqs
                .traverse( item => currentAnalyses.get( item ).flatMap( _.left.toOption ) )
                .map( _.items.mapFilter( _.items.minimumOption ).maximumOption.getOrElse( Milestone.Zero ) )
                .cata(
                  milestone => ( true, Left( milestone ) ),
                  ( false, Right( reqs ) )
                )

      if ( progressed ) loop( newAnalyses )
      else
        val (
          notAnalyzed: Vector[ClassName[Any]],
          result: (
              Map[ClassName[GameRecipe], Milestone],
              Map[ClassName[Extractor], Milestone],
              Map[ClassName[PowerGenerator], Milestone]
          )
        ) =
          (
            finishAnalysis( newAnalyses.recipes ),
            finishAnalysis( newAnalyses.extractors ),
            finishAnalysis( newAnalyses.powerGenerators )
          ).tupled( using cats.Invariant.catsStdMonadForTuple2, cats.Invariant.catsStdMonadForTuple2 ) // Huh?

        // good enough
        notAnalyzed.foreach( na => println( show"INFO [NOT ANALYZED] $na" ) )

        result

    def initAnalyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]] =
      Analyses(
        data.schematics
          .mapFilter( schematic => analyzeSchematic( schematic ).tupleLeft( schematic.className ) )
          .to( SortedMap ),
        data.recipes.map( recipe => ( recipe.className, analyzeRecipe( recipe ) ) ).to( SortedMap ),
        analyzeGenerators,
        analyzeExtractors
      )

    def run: Result =
      val (
        recipeAnalysis: Map[ClassName[GameRecipe], Milestone],
        extractorAnalysis: Map[ClassName[Extractor], Milestone],
        powerGeneratorAnalysis: Map[ClassName[PowerGenerator], Milestone]
      ) = loop( initAnalyses )

      val recipeCategories: Map[ClassName[GameRecipe], RecipeCategory] =
        manufacturingRecipes
          .mapFilter: recipe =>
            val tier: Tier = recipeAnalysis.getOrElse( recipe.className, Milestone.Zero ).tier

            recipeSchematics
              .get( recipe.className )
              .flatMap[RecipeCategory]( s =>
                s.`type` match
                  case SchematicType.Milestone | SchematicType.Tutorial | SchematicType.Custom |
                      SchematicType.Customization =>
                    Some( RecipeCategory.Milestone( tier ) )
                  case SchematicType.Alternate =>
                    Some( RecipeCategory.Alternate( tier ) )
                  case SchematicType.Mam =>
                    MilestoneAnalyzer.researchCategoryOf( s ).map( RecipeCategory.Mam( tier, _ ) )
                  case SchematicType.HardDrive | SchematicType.Shop => None
              )
              .tupleLeft( recipe.className )
          .toMap

      Result( recipeCategories, powerGeneratorAnalysis.fmap( _.tier ), extractorAnalysis.fmap( _.tier ) )

  object MilestoneAnalyzer:
    private def alterClassName[A]( manufacturerClass: ClassName[A] ): ClassName[GameItem] =
      ClassName( "Desc_" + manufacturerClass.name.stripPrefix( "Build_" ) )

    private def researchCategoryOf( schematic: Schematic ): Option[ResearchCategory] =
      ResearchCategory.cases
        .find( rc => rc.keys.exists( k => schematic.className.name.startsWith( s"Research_${k}_" ) ) )
        .filter( _ => !schematic.displayName.toLowerCase.startsWith( "discontinued" ) )

    private def canonicalUnlocks( data: GameData ): Map[ClassName[GameRecipe], Schematic] =
      val allRecipeUnlocks: Map[ClassName[GameRecipe], NonEmptyVector[Schematic]] =
        data.schematics.foldMap( s => s.unlocks.tupleRight( NonEmptyVector.one( s ) ).toMap )

      def schematicPriority( schematic: Schematic ) =
        schematic.`type` match
          case SchematicType.Mam           => researchCategoryOf( schematic ).fold( 99 )( _ => 0 )
          case SchematicType.Milestone     => 1
          case SchematicType.Tutorial      => 1
          case SchematicType.Custom        => 2
          case SchematicType.Customization => 3
          case SchematicType.Alternate     => 4
          case SchematicType.HardDrive     => 99
          case SchematicType.Shop          => 99

      allRecipeUnlocks.fmap( _.minimumBy( schematicPriority ) )

    def init: MilestoneAnalyzer =
      val manufacturingRecipes: Vector[GameRecipe] = data.recipes.filter( recipe =>
        recipe.producedIn.intersect[ClassName[Manufacturer]]( data.manufacturers.keys.toSeq ).size == 1
      )

      val manufacturingRecipeClasses: Set[ClassName[GameRecipe]] = manufacturingRecipes.map( _.className ).toSet

      val recipeSchematics: Map[ClassName[GameRecipe], Schematic] =
        canonicalUnlocks( data )
          .filter:
            case ( c, _ ) => manufacturingRecipeClasses.contains( c )

      val schematicDependencies: Map[ClassName[Schematic], AllOf[Schematic]] =
        val schematicsByClassName: Map[ClassName[Schematic], Schematic] =
          data.schematics.map( s => ( s.className, s ) ).toMap

        def dependenciesOf( schematic: Schematic ): Vector[Schematic] =
          schematic.schematicDependencies
            .mapFilter( schematicsByClassName.get )

        data.schematics
          .filter( _.`type` == SchematicType.Alternate )
          .map( schematic =>
            (
              schematic.className,
              if ( schematic.requireAllDependencies )
                AllOf.allOf( dependenciesOf( schematic ) )
              else
                AllOf.anyOf( dependenciesOf( schematic ) )
            )
          )
          .toMap

      val alternateUnlocks: Map[ClassName[Schematic], GameRecipe] =
        val recipesByClassName = data.recipes.map( recipe => ( recipe.className, recipe ) ).toMap

        data.schematics
          .filter( _.`type` == SchematicType.Alternate )
          .mapFilter: schematic =>
            schematic.unlocks
              .mapFilter( recipesByClassName.get )
              .toNev
              .map( _.head )
              .tupleLeft( schematic.className )
          .toMap

      val noBaseRecipes: Set[ClassName[GameItem]] =
        data.items.values
          .collect:
            case item if item.nativeClass == NativeClass.resourceDescClass && item.form == GameForm.Solid =>
              item.className
          .toSet

      val baseRecipes: Map[ClassName[GameItem], GameRecipe] =
        manufacturingRecipes
          .filter( recipe =>
            !recipe.displayName.toLowerCase.startsWith( "alternate" ) &&
              recipeSchematics.get( recipe.className ).forall( _.`type` != SchematicType.Alternate )
          )
          .map( recipe => ( recipe.products.head.item, recipe ) )
          .filterNot:
            case ( item, _ ) => noBaseRecipes.contains( item )
          .toMap

      def schematicsOf[A]( things: Vector[ClassName[A]] ): Map[ClassName[A], Schematic] =
        things
          .mapFilter: thing =>
            data.recipes
              .find( _.products.exists( _.item == alterClassName( thing ) ) )
              .mapFilter: recipe =>
                data.schematics.find( _.unlocks.contains( recipe.className ) )
              .tupleLeft( thing )
          .toMap

      val manufacturerSchematics: Map[ClassName[Manufacturer], Schematic] =
        schematicsOf( data.manufacturers.values.map( _.className ).toVector )

      val powerGeneratorSchematics: Map[ClassName[PowerGenerator], Schematic] =
        schematicsOf( data.powerGenerators.values.map( _.className ).toVector )

      val extractorSchematics: Map[ClassName[Extractor], Schematic] =
        schematicsOf( data.extractors.values.map( _.className ).toVector )

      new MilestoneAnalyzer(
        manufacturingRecipes,
        recipeSchematics,
        schematicDependencies,
        alternateUnlocks,
        baseRecipes,
        manufacturerSchematics,
        powerGeneratorSchematics,
        extractorSchematics
      )

object RecipeClassifier:

  opaque type OneOf[A] = Vector[A]
  object OneOf:
    def one[A]( item: A ): OneOf[A]                       = Vector( item )
    inline def apply[A]( items: Vector[A] ): OneOf[A]     = items
    given Traverse[OneOf]                                 = Traverse[Vector]
    given oneOfMonoidK: MonoidK[OneOf]                    = MonoidK[Vector]
    given [A] => Monoid[OneOf[A]]                         = oneOfMonoidK.algebra
    extension [A]( oneOf: OneOf[A] ) def items: Vector[A] = oneOf

  opaque type AllOf[A] = Nested[Vector, OneOf, A]
  object AllOf:
    def one[A]( item: A ): AllOf[A]                              = allOf( Vector( item ) )
    def allOf[A]( items: Vector[A] ): AllOf[A]                   = items.map( OneOf.one ).nested
    def anyOf[A]( items: Vector[A] ): AllOf[A]                   = Vector( OneOf( items ) ).nested
    inline def apply[A]( items: Vector[OneOf[A]] ): AllOf[A]     = items.nested
    given Traverse[AllOf]                                        = Traverse[Nested[Vector, OneOf, *]]
    given allOfMonoidK: MonoidK[AllOf]                           = MonoidK[Nested[Vector, OneOf, *]]
    given [A] => Monoid[AllOf[A]]                                = allOfMonoidK.algebra
    extension [A]( allOf: AllOf[A] ) def items: Vector[OneOf[A]] = allOf.value

  opaque type Milestone = Tier
  object Milestone:
    val Zero: Milestone                               = Tier( 0 )
    def apply( schematic: Schematic ): Milestone      = Tier( schematic.techTier )
    given Order[Milestone]                            = Order[Tier]
    given Ordering[Milestone]                         = Order.catsKernelOrderingForOrder
    extension ( milestone: Milestone ) def tier: Tier = milestone

  enum AnalysisItem:
    override def toString: String = show"${this.displayName} [${this.className}]"
    case OfRecipe( recipe: GameRecipe )      extends AnalysisItem
    case OfSchematic( schematic: Schematic ) extends AnalysisItem

  object AnalysisItem:
    extension ( analysisItem: AnalysisItem )
      def className: ClassName[Any] = analysisItem match
        case OfRecipe( recipe )       => recipe.className
        case OfSchematic( schematic ) => schematic.className
      def displayName: String = analysisItem match
        case OfRecipe( recipe )       => recipe.displayName
        case OfSchematic( schematic ) => schematic.displayName

    given Show[AnalysisItem] = Show.fromToString

  case class Analyses[A](
      schematics: SortedMap[ClassName[Schematic], A],
      recipes: SortedMap[ClassName[GameRecipe], A],
      powerGenerators: SortedMap[ClassName[PowerGenerator], A],
      extractors: SortedMap[ClassName[Extractor], A]
  ) derives Traverse:
    def get( item: AnalysisItem ): Option[A] =
      item match
        case AnalysisItem.OfRecipe( recipe )       => recipes.get( recipe.className )
        case AnalysisItem.OfSchematic( schematic ) => schematics.get( schematic.className )

  case class Result(
      recipes: Map[ClassName[GameRecipe], RecipeCategory],
      powerGenerators: Map[ClassName[PowerGenerator], Tier],
      extractors: Map[ClassName[Extractor], Tier]
  )
