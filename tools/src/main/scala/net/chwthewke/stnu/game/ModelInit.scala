package net.chwthewke.stnu
package game

import alleycats.std.map.*
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

import data.Countable
import model.ExtractorType
import model.Form
import model.Item
import model.Machine
import model.MachineType
import model.ManufacturerType
import model.Model
import model.Power
import model.Recipe
import model.RecipeCategory
import model.ResourceOptions
import model.ResourcePurity
import model.ResourceWeights
import model.Transport

opaque type ModelItems = Map[ClassName[Item], Item]
object ModelItems:
  inline def apply( items: Map[ClassName[Item], Item] ): ModelItems = items
  extension ( modelItems: ModelItems )
    def items: Map[ClassName[Item], Item] = modelItems
    def get( className: ClassName[Item] ): ValidatedNel[String, Item] =
      modelItems.get( className ).toValidNel( "No such item: " + className )

object ModelInit:

  extension [A]( className: ClassName[A] ) inline def translate[B]: ClassName[B] = ClassName( className.name )

  extension ( data: GameData )
    def getItem( className: ClassName[GameItem] ): ValidatedNel[String, GameItem] =
      data.items
        .get( className )
        .toValidNel( show"No such item class: $className" )

  def apply( version: ModelVersion, data: GameData, mapConfig: MapConfig ): ValidatedNel[String, Model] =
    extractItems( data ).andThen: modelItems =>

      val ( rawSelfExtraction, rawManufacturing ) = data.recipes.partition( _.isSelfExtraction )

      val extractorMachines: ValidatedNel[String, Map[ClassName[Extractor], ( Extractor, Machine )]] =
        data.extractors.traverse( ex => extractorMachine( ex ).toValidatedNel.tupleLeft( ex ) )

      val extractionRecipes: ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe.Prod )]] =
        extractorMachines.andThen( getExtractionRecipes( data, modelItems, _, rawSelfExtraction ) )

      val manufacturingRecipeClassification: Map[ClassName[GameRecipe], RecipeCategory] =
        RecipeClassifier( data ).classifyRecipes

      val manufacturing: ValidatedNel[String, Vector[Recipe.Prod]] =
        rawManufacturing
          .traverseFilter( validateManufacturingRecipe( data, modelItems, manufacturingRecipeClassification, _ ) )

      val powerRecipes: ValidatedNel[String, Vector[Recipe.PowerGen]] =
        extractPowerRecipes( data, modelItems )

      val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
        initResourceOptions( data.items, mapConfig ).toValidatedNel

      (
        extractionRecipes,
        manufacturing,
        powerRecipes,
        validateTransports( data.conveyorBelts ),
        validateTransports( data.pipelines ),
        defaultResourceOptions
      )
        .mapN: ( ex, mf, pw, cb, pp, ro ) =>

          val usefulItemClasses: Set[ClassName[Item]] =
            ex.foldMap( t => itemClassesOf( t._3 ) ) ++
              mf.foldMap( itemClassesOf( _ ) ) ++
              pw.foldMap( itemClassesOf( _ ) )

          val usefulItems: SortedMap[ClassName[Item], Item] =
            modelItems.items
              .filter:
                case ( cn, _ ) => usefulItemClasses.contains( cn )
              .to( SortedMap )

          val machines: SortedMap[ClassName[Machine], Machine] =
            ( ex.map( _._3.producedIn ) ++ mf.map( _.producedIn ) ++ pw.map( _.producedIn ) )
              .fproductLeft( _.className )
              .to( SortedMap )

          Model( version, usefulItems, ex.map( _._1 ).distinct, mf, pw, ex, machines, cb, pp, ro )

  private def itemClassesOf[P[_]: Traverse]( recipe: Recipe ): Set[ClassName[Item]] =
    recipe.itemsPerMinuteMap.keySet.map( _.className )

  private def validateTransports( gameTransports: Vector[LogisticsData] ): ValidatedNel[String, Vector[Transport]] =
    gameTransports
      .map:
        case LogisticsData( className, displayName, amountPerMinute ) =>
          Transport( className.translate, displayName, amountPerMinute )
      .validNel

  def initResourceOptions(
      modelItems: Map[ClassName[GameItem], GameItem],
      config: MapConfig
  ): Either[String, ResourceOptions] =
    config.resourceNodes
      .traverse:
        _.toVector
          .traverse:
            case ( itemClass, distrib ) =>
              modelItems.get( itemClass.translate ).toValidNel( itemClass ).as( itemClass ).tupleRight( distrib )
          .map( _.toMap )
      .map( ResourceOptions( _, ResourceWeights.default ) )
      .leftMap( _.mkString_( "Unknown items in resource nodes config: ", ", ", "" ) )
      .toEither

  def extractItems( data: GameData ): ValidatedNel[String, ModelItems] =
    data.items.values.toVector
      .traverse( validateItem )
      .map: items =>
        ModelItems( items.fproductLeft( _.className ).toMap )

  def validateItem( item: GameItem ): ValidatedNel[String, Item] =
    ( item.form match
      case GameForm.Solid   => Form.Solid.validNel
      case GameForm.Liquid  => Form.Liquid.validNel
      case GameForm.Gas     => Form.Gas.validNel
      case GameForm.Invalid => s"Invalid form for ${item.displayName} # ${item.className}".invalidNel
    ).map: form =>
      Item(
        item.className.translate,
        item.displayName,
        form,
        item.fuelValue,
        item.sinkPoints
      )

  def validateRecipeItem(
      data: GameData,
      modelItems: ModelItems,
      ccn: Countable[Double, ClassName[GameItem]]
  ): ValidatedNel[String, Countable[Double, Item]] =
    (
      data.getItem( ccn.item ),
      modelItems.get( ccn.item.translate )
    )
      .mapN: ( gameItem, item ) =>
        Countable( item, ccn.amount / gameItem.form.simpleAmountFactor )

  def validateRecipeItems[F[_]: Traverse](
      data: GameData,
      modelItems: ModelItems,
      items: F[Countable[Double, ClassName[GameItem]]]
  ): ValidatedNel[String, F[Countable[Double, Item]]] =
    items.traverse( validateRecipeItem( data, modelItems, _ ) )

  def getExtractionRecipes(
      data: GameData,
      modelItems: ModelItems,
      machines: Map[ClassName[Extractor], ( Extractor, Machine )],
      selfExtraction: Vector[GameRecipe]
  ): ValidatedNel[String, Vector[( Item, ResourcePurity, Recipe.Prod )]] =
    val ( miners, otherExtractors ) =
      machines.values.toVector.partition( _._2.machineType.is( ExtractorType.Miner ) )

    (
      getMinerProducts( data, modelItems, miners, selfExtraction ),
      getOtherExtractionProducts( data, modelItems, otherExtractors )
    ).mapN( _ ++ _ )
      .map:
        _.flatMap:
          case ( gameItem, item, extractor, machine ) =>
            ResourcePurity.cases
              .map( purity => ( item, purity, extractionRecipe( gameItem, item, extractor, purity, machine ) ) )

  def getMinerProducts(
      data: GameData,
      modelItems: ModelItems,
      converterExtractors: Vector[( Extractor, Machine )],
      selfExtraction: Vector[GameRecipe]
  ): ValidatedNel[String, Vector[( GameItem, Item, Extractor, Machine )]] =
    val selfExtractionProducts: ValidatedNel[String, Vector[GameItem]] =
      selfExtraction
        .traverseFilter: r =>
          data
            .getItem( r.products.head.item )
            .map( item => Option.when( item.form == GameForm.Solid )( item ) )

    val ores: ValidatedNel[String, Vector[GameItem]] =
      data.items.values
        .collect:
          case item if item.nativeClass == NativeClass.resourceDescClass && item.form == GameForm.Solid => item
        .toVector
        .validNel

    ( selfExtractionProducts, ores )
      .mapN( _ ++ _ )
      .map( _.distinctBy( _.className ) )
      .andThen:
        _.traverse: item =>
          modelItems.get( item.className.translate ).tupleLeft( item )
      .map: ores =>
        for
          ( extractor, machine ) <- converterExtractors
          ( gameItem, item )     <- ores
        yield ( gameItem, item, extractor, machine )

  def getOtherExtractionProducts(
      data: GameData,
      modelItems: ModelItems,
      extractors: Vector[( Extractor, Machine )]
  ): ValidatedNel[String, Vector[( GameItem, Item, Extractor, Machine )]] =
    ( for
      ( extractor, machine ) <- extractors
      allowedResources       <- extractor.allowedResources.toVector
      resource               <- allowedResources.toList.toVector
    yield ( resource, extractor, machine ) )
      .traverse:
        case ( resource, extractor, machine ) =>
          ( data.getItem( resource ), modelItems.get( resource.translate ) ).mapN( ( _, _, extractor, machine ) )

  def extractorMachine( extractor: Extractor ): Either[String, Machine] =
    ExtractorType.cases
      .find( _.dataKey.fold( _ == extractor.extractorTypeName, _ == extractor.className ) )
      .toRight( s"No known extractor type for class ${extractor.className}, type ${extractor.extractorTypeName}" )
      .map( exType =>
        Machine(
          extractor.className.translate,
          extractor.displayName,
          MachineType( exType ),
          extractor.powerConsumption,
          extractor.powerConsumptionExponent
        )
      )

  def extractionRecipe(
      gameItem: GameItem,
      item: Item,
      extractor: Extractor,
      purity: ResourcePurity,
      machine: Machine
  ): Recipe.Prod =
    Recipe.Prod(
      ClassName( show"${item.className}_${purity.entryName.capitalize}_${extractor.className}" ),
      show"${item.displayName} ($purity, ${extractor.displayName})",
      RecipeCategory.Extraction,
      Nil,
      NonEmptyList.of(
        Countable( item, extractor.itemsPerCycle.toDouble / gameItem.form.simpleAmountFactor * purity.multiplier )
      ),
      extractor.cycleTime,
      machine,
      Power.Fixed( extractor.powerConsumption )
    )

  def manufacturerMachine( manufacturer: Manufacturer ): Machine =
    Machine(
      manufacturer.className.translate,
      manufacturer.displayName,
      MachineType(
        if ( manufacturer.isCollider ) ManufacturerType.VariableManufacturer
        else ManufacturerType.Manufacturer
      ),
      manufacturer.powerConsumption,
      manufacturer.powerConsumptionExponent
    )

  def validateManufacturer( data: GameData, className: ClassName[Manufacturer] ): ValidatedNel[String, Machine] =
    data.manufacturers
      .get( className )
      .map( manufacturerMachine )
      .toValidNel( show"Unknown machine class $className" )

  def recipePower( recipe: GameRecipe, manufacturer: Machine ): Power =
    if ( manufacturer.machineType.manufacturer.contains( ManufacturerType.VariableManufacturer ) )
      Power.Variable( recipe.variablePowerMin, recipe.variablePowerMin + recipe.variablePowerRange )
    else
      Power.Fixed( manufacturer.powerConsumption )

  def validateManufacturingRecipe(
      data: GameData,
      modelItems: ModelItems,
      classification: Map[ClassName[GameRecipe], RecipeCategory],
      recipe: GameRecipe
  ): ValidatedNel[String, Option[Recipe.Prod]] =
    NonEmptyList
      .fromList( recipe.producedIn.filter( data.manufacturers.keySet ) )
      .traverse( ms =>
        (
          classification
            .get( recipe.className )
            .toValidNel( show"Recipe ${recipe.displayName} [${recipe.className}] not classified" ),
          Option
            .when( ms.size == 1 )( ms.head )
            .toValidNel(
              show"Recipe ${recipe.displayName} [${recipe.className}] is produced in multiple manufacturers"
            )
            .andThen( validateManufacturer( data, _ ) ),
          validateRecipeItems( data, modelItems, recipe.ingredients ),
          validateRecipeItems( data, modelItems, recipe.products )
        ).mapN( ( cat, producer, ingredients, products ) =>
          Recipe.Prod(
            recipe.className.translate,
            recipe.displayName,
            cat,
            ingredients,
            products,
            recipe.duration,
            producer,
            recipePower( recipe, producer )
          )
        )
      )

  def extractPowerRecipes( gameData: GameData, modelItems: ModelItems ): ValidatedNel[String, Vector[Recipe.PowerGen]] =
    gameData.powerGenerators.values.toVector.foldMapA: generator =>
      generator.fuels.traverse: fuel =>
        (
          modelItems.get( fuel.fuel.translate ),
          fuel.byproduct.traverse( _.traverse( cn => modelItems.get( cn.translate ) ) ),
          fuel.supplementalResource.traverse( cn => modelItems.get( cn.translate ) )
        ).mapN: ( f, bp, so ) =>
          val powerGenMW: Frac  = Frac.decimal( generator.powerProduction )
          val fuelValueMJ: Frac = Frac.decimal( f.fuelValue )

          val Frac( fAm, durMs ) = powerGenMW / ( 1000 *: fuelValueMJ )
          def sAm: Double        = fAm * f.fuelValue * generator.supplementalToPowerRatio / 1000

          Recipe.PowerGen(
            ClassName( s"${generator.className.name}__${f.className.name}" ),
            show"${f.displayName} in ${generator.displayName}",
            RecipeCategory.NuclearWaste,
            Countable( f, fAm.toDouble ) :: so.map( Countable( _, sAm ) ).toList,
            bp.map( _.mapAmount( fAm.toDouble * _ ) ).toList,
            durMs.milliseconds,
            Machine(
              generator.className.translate,
              generator.displayName,
              MachineType( ManufacturerType.Manufacturer ),
              0d,
              generator.powerConsumptionExponent
            ),
            Power.Fixed( -generator.powerProduction )
          )
