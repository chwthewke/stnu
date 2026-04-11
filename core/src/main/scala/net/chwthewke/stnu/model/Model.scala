package net.chwthewke.stnu
package model

import alleycats.std.iterable.*
import cats.Show
import cats.Traverse
import cats.data.NonEmptyVector
import cats.data.ReaderT
import cats.data.ValidatedNel
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

import data.Countable

case class Model(
    version: ModelVersion,
    items: SortedMap[ClassName[Item], Item],
    extractedItems: Vector[Item],
    manufacturingRecipes: Vector[Recipe.Manufacturing],
    powerRecipes: Vector[Recipe.PowerGeneration],
    extractionRecipes: SortedMap[( Item, Machine ), ExtractionRecipes],
    machines: SortedMap[ClassName[Machine], Machine],
    conveyorBelts: NonEmptyVector[Transport],
    pipelines: NonEmptyVector[Transport],
    defaultResourceOptions: ResourceOptions
):
  lazy val recipes: SortedMap[ClassName[Recipe], Recipe] =
    ( manufacturingRecipes ++ powerRecipes ++ extractionRecipes.foldMap( _.recipes ) )
      .fproductLeft( _.className )
      .to( SortedMap )

  def withItems( items: SortedMap[ClassName[Item], Item] ): Either[String, Model] =
    Model.Compact( this ).copy( items = items.values.toVector ).model

  def resourceCaps(
      minerClass: ClassName[Machine],
      clockSpeed: ClockSpeedPreset,
      extractors: Set[ExtractorType],
      resourceNodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]]
  ): SortedMap[ClassName[Item], Option[Double]] =
    def areExtractionRecipesAllowed( recipes: ExtractionRecipes ): Boolean =
      recipes.recipes.headOption.exists: recipe =>
        recipe.producedIn.machineType.extractor.exists:
          case ExtractorType.Miner => recipe.producedIn.className == minerClass
          case other               => extractors.contains( other )

    val extractionRecipesByItem: SortedMap[ClassName[Item], Vector[( Machine, ExtractionRecipes )]] =
      extractionRecipes.toVector
        .filter { case ( _, recipes ) => areExtractionRecipesAllowed( recipes ) }
        .foldMap:
          case ( ( item, machine ), recipes ) => SortedMap( item.className -> Vector( ( machine, recipes ) ) )

    extractionRecipesByItem.map:
      case ( item, machineRecipes ) =>
        item ->
          machineRecipes
            .foldMapM:
              case ( _, ExtractionRecipes.Fixed( _ ) )                 => none
              case ( machine, ExtractionRecipes.Variable( byPurity ) ) =>
                machine.machineType.extractor
                  .flatMap( resourceNodes.get )
                  .flatMap( _.get( item ) )
                  .foldMap: distrib =>
                    (
                      clockSpeed.value.fraction *
                        distrib.foldMap( ( purity, count ) => count * byPurity.get( purity ).productsPerMinute.amount )
                    ).some

object Model:

  given Show[Model] = Show.show: model =>
    show"""Manufacturing Recipes
          |${model.manufacturingRecipes.mkString_( "\n" )}
          |
          |Items
          |${model.items.values.map( _.toString ).intercalate( "\n" )}
          |
          |Extracted Items ${model.extractedItems.map( _.displayName ).intercalate( ", " )}
          |
          |Extraction Recipes
          |${model.extractionRecipes.foldMap( _.recipes ).mkString_( "\n" )}
          |
          |Resource nodes
          |${model.defaultResourceOptions.show.linesIterator.map( "  " + _ ).toSeq.mkString_( "\n" )}
          |""".stripMargin

  private given Decoder[FiniteDuration] = Decoder[Long].map( _.millis )
  private given Encoder[FiniteDuration] = Encoder[Long].contramap( _.toMillis )

  private object Types:
    opaque type Index = ( Map[ClassName[Item], Item], Map[ClassName[Machine], Machine] )
    object Index:
      def apply( items: Map[ClassName[Item], Item], machines: Map[ClassName[Machine], Machine] ): Index =
        ( items, machines )
      extension ( index: Index )
        def item( className: ClassName[Item] ): ValidatedNel[String, Item] =
          index._1.get( className ).toValidNel( show"No such item class $className" )
        def machine( className: ClassName[Machine] ): ValidatedNel[String, Machine] =
          index._2.get( className ).toValidNel( show"No such machine class $className" )

      def item( className: ClassName[Item] ): ReaderT[ValidatedNel[String, *], Types.Index, Item] =
        ReaderT( index => index.item( className ) )
      def machine( className: ClassName[Machine] ): ReaderT[ValidatedNel[String, *], Types.Index, Machine] =
        ReaderT( index => index.machine( className ) )

  private def validateRecipeCategory[C <: RecipeCategory]( p: RecipeCategory => Option[C] )(
      category: RecipeCategory
  ): ValidatedNel[String, C] =
    p( category ).toValidNel( s"invalid category $category" )

  private case class CompactRecipe(
      className: ClassName[Recipe],
      displayName: String,
      category: RecipeCategory,
      ingredients: List[Countable[Double, ClassName[Item]]],
      products: List[Countable[Double, ClassName[Item]]],
      duration: FiniteDuration,
      producedIn: ClassName[Machine],
      purity: Option[ResourcePurity],
      power: Power
  ) derives Show:
    def manufacturing: ReaderT[ValidatedNel[String, *], Types.Index, Recipe.Manufacturing] = ReaderT: index =>
      (
        validateRecipeCategory( _.manufacturing )( category ).tag( show"Category $category" ),
        ingredients.traverse( _.traverse( index.item( _ ).tag( "Ingredient" ) ) ),
        products.toNel
          .toValidNel( show"Recipe with empty products $className" )
          .andThen:
            _.traverse( _.traverse( index.item ).tag( "Product" ) )
        ,
        index.machine( producedIn ).tag( "Producer" )
      ).mapN(
        Recipe.Manufacturing( className.narrow[Recipe.Manufacturing], displayName, _, _, _, duration, _, power )
      ).tag( s"Manufacturing $displayName ($className)" )

    def extraction: ReaderT[ValidatedNel[String, *], Types.Index, Recipe.Extraction] = ReaderT: index =>
      (
        validateRecipeCategory( _.extraction )( category ),
        ingredients.traverse( _.traverse( index.item ) ),
        products match
          case product :: Nil => product.traverse( index.item )
          case _              => show"Extraction recipe with 0 or 2+ products $className".invalidNel,
        index.machine( producedIn )
      ).mapN(
        Recipe.Extraction( className.narrow[Recipe.Extraction], displayName, _, _, _, duration, _, purity, power )
      ).tag( s"Extraction $displayName ($className)" )

    def powerGeneration: ReaderT[ValidatedNel[String, *], Types.Index, Recipe.PowerGeneration] =
      ReaderT: index =>
        (
          validateRecipeCategory( _.powerGeneration )( category ),
          ingredients.traverse( _.traverse( index.item ) ),
          products.traverse( _.traverse( index.item ) ),
          index.machine( producedIn )
        ).mapN(
          Recipe.PowerGeneration(
            className.narrow[Recipe.PowerGeneration],
            displayName,
            _,
            _,
            _,
            duration,
            _,
            power
          )
        ).tag( s"PowerGeneration $displayName ($className)" )

  private object CompactRecipe:
    def of( recipe: Recipe )( using Traverse[recipe.P] ): CompactRecipe =
      CompactRecipe(
        recipe.className,
        recipe.displayName,
        recipe.category,
        recipe.ingredients.map( _.map( _.className ) ),
        recipe.products.map( _.map( _.className ) ).toList,
        recipe.duration,
        recipe.producedIn.className,
        recipe match
          case e: Recipe.Extraction => e.purity
          case _                    => none,
        recipe.power
      )

    given Decoder[CompactRecipe] = ConfiguredDecoder.derive[CompactRecipe]()
    given Encoder[CompactRecipe] = ConfiguredEncoder.derive[CompactRecipe]().mapJson( _.dropNullValues )

  private case class CompactExtractionRecipes( recipes: Vector[CompactRecipe] )
      derives Show,
        ConfiguredDecoder,
        ConfiguredEncoder:
    def extractionRecipes: ReaderT[ValidatedNel[String, *], Types.Index, ExtractionRecipes] =
      recipes match
        case Vector( recipe ) => recipe.extraction.map( ExtractionRecipes.Fixed( _ ) )

        case v if v.size == ResourcePurity.cases.size =>
          recipes
            .zip( ResourcePurity.cases )
            .traverse:
              case ( recipe, purity ) => recipe.extraction.tupleLeft( purity )
            .mapF: recipesV =>
              recipesV.andThen: recipes =>
                ExtractionRecipes.ByPurity( recipes )

        case _ =>
          ReaderT.liftF( s"CompactExtractionRecipes must have 1 or ${ResourcePurity.cases.size} elements".invalidNel )

  private object CompactExtractionRecipes:
    def of( extractionRecipes: ExtractionRecipes ): CompactExtractionRecipes =
      CompactExtractionRecipes( extractionRecipes.recipes.map( CompactRecipe.of( _ ) ) )

  extension [A]( validated: ValidatedNel[String, A] )
    private def tag( t: String ): ValidatedNel[String, A] =
      validated.leftMap( _.map( err => s"[$t $err]" ) )

  private case class Compact(
      version: ModelVersion,
      items: Vector[Item],
      extractedItems: Vector[ClassName[Item]],
      manufacturingRecipes: Vector[CompactRecipe],
      powerRecipes: Vector[CompactRecipe],
      extractionRecipes: Vector[( ClassName[Item], ClassName[Machine], CompactExtractionRecipes )],
      machines: Vector[Machine],
      conveyorBelts: NonEmptyVector[Transport],
      pipelines: NonEmptyVector[Transport],
      defaultResourceOptions: ResourceOptions
  ) derives Show,
        ConfiguredDecoder,
        ConfiguredEncoder:
    def model: Either[String, Model] =
      val itemsMap: SortedMap[ClassName[Item], Item]          = items.fproductLeft( _.className ).to( SortedMap )
      val machinesMap: SortedMap[ClassName[Machine], Machine] = machines.fproductLeft( _.className ).to( SortedMap )
      (
        ReaderT( ( index: Types.Index ) => extractedItems.traverse( index.item( _ ).tag( "Extracted" ) ) ),
        manufacturingRecipes.traverse( _.manufacturing ),
        powerRecipes.traverse( _.powerGeneration ),
        extractionRecipes
          .traverse:
            case ( itemClass, machineClass, recipes ) =>
              (
                ( Types.Index.item( itemClass ), Types.Index.machine( machineClass ) ).tupled,
                recipes.extractionRecipes
              ).tupled
          .map( _.to( SortedMap ) )
      ).mapN( Model( version, itemsMap, _, _, _, _, machinesMap, conveyorBelts, pipelines, defaultResourceOptions ) )
        .run( Types.Index( itemsMap, machinesMap ) )
        .leftMap( _.mkString_( "Model encoding errors: ", ", ", "" ) )
        .toEither

  private object Compact:
    def apply( model: Model ): Compact =
      Compact(
        model.version,
        model.items.values.toVector,
        model.extractedItems.map( _.className ),
        model.manufacturingRecipes.map( CompactRecipe.of( _ ) ),
        model.powerRecipes.map( CompactRecipe.of( _ ) ),
        model.extractionRecipes.toVector.map:
          case ( ( item, machine ), recipes ) =>
            ( item.className, machine.className, CompactExtractionRecipes.of( recipes ) ),
        model.machines.values.toVector,
        model.conveyorBelts,
        model.pipelines,
        model.defaultResourceOptions
      )

  given Decoder[Model] = Decoder[Compact].emap( _.model )
  given Encoder[Model] = Encoder[Compact].contramap( Compact.apply )
