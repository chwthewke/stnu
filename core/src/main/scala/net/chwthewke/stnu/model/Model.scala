package net.chwthewke.stnu
package model

import alleycats.std.iterable.*
import cats.Show
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.ReaderT
import cats.data.ValidatedNel
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*

import data.Countable

case class Model(
    version: ModelVersion,
    items: SortedMap[ClassName[Item], Item],
    extractedItems: Vector[Item],
    manufacturingRecipes: Vector[Recipe.Prod],
    powerRecipes: Vector[Recipe.PowerGen],
    extractionRecipes: SortedMap[( Item, Machine ), ExtractionRecipes],
    machines: SortedMap[ClassName[Machine], Machine],
    conveyorBelts: Vector[Transport],
    pipelines: Vector[Transport],
    defaultResourceOptions: ResourceOptions
):
  lazy val recipes: Map[ClassName[Recipe], Recipe] =
    ( manufacturingRecipes ++ powerRecipes ++ extractionRecipes.foldMap( _.recipes ) ).fproductLeft( _.className ).toMap

  lazy val masked: Model = Model.maskExcluded( this )

object Model:
  private val excludedProducts: Vector[String] =
    Vector(
      "biofuel",
      "gas nobelisk",
      "biomass",
      "alien protein",
      "alien dna"
    )

  private def maskExcluded( model: Model ): Model =
    val excludedItems: SortedSet[ClassName[Item]] =
      model.items
        .filter:
          case ( cn, item ) =>
            excludedProducts.exists( item.displayName.toLowerCase.contains ) &&
            !model.extractedItems.exists( _.className == cn )
        .keySet
    def exludeRecipe( recipe: Recipe ): Boolean =
      recipe.itemsPerMinute.exists( ci => excludedItems.contains( ci.item.className ) )
    val allowedManufacturingRecipes: Vector[Recipe.Prod] =
      model.manufacturingRecipes.filterNot( exludeRecipe )
    val allowedPowerRecipes: Vector[Recipe.PowerGen] =
      model.powerRecipes.filterNot( exludeRecipe )

    Model(
      model.version,
      model.items.removedAll( excludedItems ),
      model.extractedItems,
      allowedManufacturingRecipes,
      allowedPowerRecipes,
      model.extractionRecipes,
      model.machines,
      model.conveyorBelts,
      model.pipelines,
      model.defaultResourceOptions
    )

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

  private case class CompactRecipe(
      className: ClassName[Recipe],
      displayName: String,
      category: RecipeCategory,
      ingredients: List[Countable[Double, ClassName[Item]]],
      products: List[Countable[Double, ClassName[Item]]],
      duration: FiniteDuration,
      producedIn: ClassName[Machine],
      power: Power
  ) derives Show,
        ConfiguredDecoder,
        ConfiguredEncoder:
    def prod: ReaderT[ValidatedNel[String, *], Types.Index, Recipe.Prod] = ReaderT: index =>
      (
        ingredients.traverse( _.traverse( index.item ) ),
        products.toNel
          .toValidNel( show"Recipe with empty products $className" )
          .andThen:
            _.traverse( _.traverse( index.item ) )
        ,
        index.machine( producedIn )
      ).mapN( Recipe.Prod( className.narrow[Recipe.Prod], displayName, category, _, _, duration, _, power ) )

    def powerGen: ReaderT[ValidatedNel[String, *], Types.Index, Recipe.PowerGen] =
      ReaderT: index =>
        (
          ingredients.traverse( _.traverse( index.item ) ),
          products.traverse( _.traverse( index.item ) ),
          index.machine( producedIn )
        ).mapN( Recipe.PowerGen( className.narrow[Recipe.PowerGen], displayName, category, _, _, duration, _, power ) )

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
        recipe.power
      )

  private case class CompactExtractionRecipes( recipes: Vector[CompactRecipe] )
      derives Show,
        ConfiguredDecoder,
        ConfiguredEncoder:
    def extractionRecipes: ReaderT[ValidatedNel[String, *], Types.Index, ExtractionRecipes] =
      recipes match
        case Vector( recipe ) => recipe.prod.map( ExtractionRecipes.Fixed( _ ) )

        case v if v.size == ResourcePurity.cases.size =>
          recipes
            .zip( ResourcePurity.cases )
            .traverse:
              case ( recipe, purity ) => recipe.prod.tupleLeft( purity )
            .mapF: recipesV =>
              recipesV.andThen: recipes =>
                ExtractionRecipes.ByPurity( recipes )

        case _ =>
          ReaderT.liftF( s"CompactExtractionRecipes must have 1 or ${ResourcePurity.cases.size} elements".invalidNel )

  private object CompactExtractionRecipes:
    def of( extractionRecipes: ExtractionRecipes ): CompactExtractionRecipes =
      CompactExtractionRecipes( extractionRecipes.recipes.map( CompactRecipe.of( _ ) ) )

  private case class Compact(
      version: ModelVersion,
      items: Vector[Item],
      extractedItems: Vector[ClassName[Item]],
      manufacturingRecipes: Vector[CompactRecipe],
      powerRecipes: Vector[CompactRecipe],
      extractionRecipes: Vector[( ClassName[Item], ClassName[Machine], CompactExtractionRecipes )],
      machines: Vector[Machine],
      conveyorBelts: Vector[Transport],
      pipelines: Vector[Transport],
      defaultResourceOptions: ResourceOptions
  ) derives Show,
        ConfiguredDecoder,
        ConfiguredEncoder:
    def model: Either[String, Model] =
      val itemsMap: SortedMap[ClassName[Item], Item]          = items.fproductLeft( _.className ).to( SortedMap )
      val machinesMap: SortedMap[ClassName[Machine], Machine] = machines.fproductLeft( _.className ).to( SortedMap )
      (
        ReaderT( ( index: Types.Index ) => extractedItems.traverse( index.item ) ),
        manufacturingRecipes.traverse( _.prod ),
        powerRecipes.traverse( _.powerGen ),
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
