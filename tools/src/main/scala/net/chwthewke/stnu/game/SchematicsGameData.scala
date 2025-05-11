package net.chwthewke.stnu
package game

import cats.data.NonEmptyVector
import cats.syntax.all.*

import model.RecipeCategory
import model.ResearchCategory

case class SchematicsGameData(
    data: GameData,
    analyzedRecipes: Vector[GameRecipe],
    recipeSchematics: Map[ClassName[GameRecipe], Schematic],
    schematicDependencies: Map[ClassName[Schematic], AllOf[Schematic]],
    alternateUnlocks: Map[ClassName[Schematic], GameRecipe],
    manufacturerSchematics: Map[ClassName[Manufacturer], Schematic],
    powerGeneratorSchematics: Map[ClassName[PowerGenerator], Schematic],
    extractorSchematics: Map[ClassName[Extractor], Schematic]
):
  def items: Map[ClassName[GameItem], GameItem]                       = data.items
  def extractors: Map[ClassName[Extractor], Extractor]                = data.extractors
  def powerGenerators: Map[ClassName[PowerGenerator], PowerGenerator] = data.powerGenerators
  def recipes: Vector[GameRecipe]                                     = data.recipes
  def schematics: Vector[Schematic]                                   = data.schematics

  def completeAnalyses( analyses: Analyses[Milestone] ): Analyses[Milestone] =
    val recipeCategories: Map[ClassName[GameRecipe], RecipeCategory] =
      analyzedRecipes
        .mapFilter: recipe =>
          val tier: Milestone = analyses.recipes.getOrElse( recipe.className, Milestone.Zero )

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
                  SchematicsGameData.researchCategoryOf( s ).map( RecipeCategory.Mam( tier, _ ) )
                case SchematicType.HardDrive | SchematicType.Shop => None
            )
            .tupleLeft( recipe.className )
        .toMap
    analyses.copy( recipeCategories = recipeCategories )

object SchematicsGameData:
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

  def init( data: GameData ): SchematicsGameData =

    val analyzedRecipes: Vector[GameRecipe] =
      data.recipes.filter: recipe =>
        val isProducedInManufacturingMachine =
          recipe.producedIn.intersect[ClassName[Manufacturer]]( data.manufacturers.keys.toSeq ).size == 1
        val producesManufacturingMachine =
          recipe.products.size == 1 &&
            data
              .buildingOfDescriptor[Manufacturer]( recipe.products.head.item )
              .exists( data.manufacturers.keySet.contains )
        isProducedInManufacturingMachine || producesManufacturingMachine

    val recipeSchematics: Map[ClassName[GameRecipe], Schematic] =
      val analyzedRecipeClasses = analyzedRecipes.map( _.className ).toSet
      canonicalUnlocks( data )
        .filter:
          case ( c, _ ) => analyzedRecipeClasses.contains( c )

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

    SchematicsGameData(
      data,
      analyzedRecipes,
      recipeSchematics,
      schematicDependencies,
      alternateUnlocks,
      manufacturerSchematics,
      powerGeneratorSchematics,
      extractorSchematics
    )
