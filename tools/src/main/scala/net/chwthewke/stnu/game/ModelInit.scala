package net.chwthewke.stnu
package game

import alleycats.std.map.*
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.ValidatedNel
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

import data.Countable
import model.ExtractionRecipes
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
import model.Tier
import model.Transport

object ModelInit:

  object Types:
    opaque type ModelItems = Map[ClassName[Item], Item]
    object ModelItems:
      inline def apply( items: Map[ClassName[Item], Item] ): ModelItems = items
      extension ( modelItems: ModelItems )
        def items: Map[ClassName[Item], Item]                             = modelItems
        def get( className: ClassName[Item] ): ValidatedNel[String, Item] =
          modelItems.get( className ).toValidNel( "No such item: " + className )

  import Types.ModelItems

  extension [A]( className: ClassName[A] ) inline def translate[B]: ClassName[B] = ClassName( className.name )

  extension ( data: GameData )
    def getItem( className: ClassName[GameItem] ): ValidatedNel[String, GameItem] =
      data.items
        .get( className )
        .toValidNel( show"No such item class: $className" )

  def apply( version: ModelVersion, data: GameData, mapConfig: MapConfig ): ValidatedNel[String, Model] =
    val classification = MilestoneAnalyzer.apply( data )
    extractItems( data, classification.items ).andThen: modelItems =>

      val extractorExtractionRecipes: ValidatedNel[String, Vector[( ( Item, Machine ), ExtractionRecipes )]] =
        data.extractors
          .traverse: ex =>
            extractorMachine( classification.extractors, ex ).map:
              case ( machine, tier ) => ( ex, machine, tier )
          .andThen( getExtractionRecipes( data, modelItems, _ ) )
          .andThen:
            _.traverse:
              case ( item, machine, byPurity ) =>
                ExtractionRecipes.ByPurity( byPurity ).map( ( ( item, machine ), _ ) )

      val simpleProducersExtraction: ValidatedNel[String, Vector[( ( Item, Machine ), ExtractionRecipes )]] =
        data.simpleProducers.traverseFilter( simpleProducerExtraction( modelItems, _ ) )

      val extractionRecipes: ValidatedNel[String, SortedMap[( Item, Machine ), ExtractionRecipes]] =
        (
          extractorExtractionRecipes,
          simpleProducersExtraction
        )
          .mapN: ( ex, sp ) =>
            ( ex ++ sp ).to( SortedMap )

      val manufacturing: ValidatedNel[String, Vector[Recipe.Manufacturing]] =
        data.recipes
          .traverseFilter( validateManufacturingRecipe( data, modelItems, classification.recipeCategories, _ ) )

      val powerRecipes: ValidatedNel[String, Vector[Recipe.PowerGeneration]] =
        extractPowerRecipes( data, modelItems, classification.powerGenerators )

      val defaultResourceOptions: ValidatedNel[String, ResourceOptions] =
        initResourceOptions( data.items, mapConfig ).toValidatedNel

      (
        extractionRecipes,
        manufacturing,
        powerRecipes,
        validateTransports( "conveyor belts" )( data.conveyorBelts ),
        validateTransports( "pipelines" )( data.pipelines ),
        defaultResourceOptions
      )
        .mapN: ( ex, mf, pw, cb, pp, ro ) =>

          val usefulItemClasses: Set[ClassName[Item]] =
            ex.foldMap( t => t.recipes.foldMap( itemClassesOf( _ ) ) ) ++
              mf.foldMap( itemClassesOf( _ ) ) ++
              pw.foldMap( itemClassesOf( _ ) )

          val usefulItems: SortedMap[ClassName[Item], Item] =
            modelItems.items
              .filter:
                case ( cn, _ ) => usefulItemClasses.contains( cn )
              .to( SortedMap )

          val machines: SortedMap[ClassName[Machine], Machine] =
            ( ex.foldMap( _.recipes.map( _.producedIn ) ) ++ mf.map( _.producedIn ) ++ pw.map( _.producedIn ) )
              .fproductLeft( _.className )
              .to( SortedMap )

          Model( version, usefulItems, ex.keySet.map( _._1 ).toVector, mf, pw, ex, machines, cb, pp, ro )

  private def itemClassesOf( recipe: Recipe ): Set[ClassName[Item]] =
    recipe.itemsPerMinuteMap.keySet.map( _.className )

  private def validateTransports(
      categoryDesc: String
  )( gameTransports: Vector[LogisticsData] ): ValidatedNel[String, NonEmptyVector[Transport]] =
    gameTransports
      .map:
        case LogisticsData( className, displayName, amountPerMinute ) =>
          Transport( className.translate, displayName, amountPerMinute )
      .toNev
      .toValidNel( s"No transports ($categoryDesc)" )

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

  def extractItems(
      data: GameData,
      classification: SortedMap[ClassName[GameItem], Tier]
  ): ValidatedNel[String, ModelItems] =
    data.items.values.toVector
      .traverse( validateItem( classification, _ ) )
      .map: items =>
        ModelItems( items.fproductLeft( _.className ).toMap )

  def validateItem( itemTiers: SortedMap[ClassName[GameItem], Tier], item: GameItem ): ValidatedNel[String, Item] =
    (
      item.form match
        case GameForm.Solid   => Form.Solid.validNel
        case GameForm.Liquid  => Form.Liquid.validNel
        case GameForm.Gas     => Form.Gas.validNel
        case GameForm.Invalid => s"Invalid form for ${item.displayName} # ${item.className}".invalidNel
      ,
      itemTiers.get( item.className ).toValidNel( s"Item ${item.displayName} [${item.className}] not classified" )
    ).mapN: ( form, tier ) =>
      Item(
        item.className.translate,
        item.displayName,
        form,
        item.fuelValue,
        item.sinkPoints,
        tier
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
      machines: Map[ClassName[Extractor], ( Extractor, Machine, Tier )]
  ): ValidatedNel[String, Vector[( Item, Machine, Vector[( ResourcePurity, Recipe.Extraction )] )]] =
    val ( miners, otherExtractors ) =
      machines.values.toVector.partition( _._2.machineType.is( ExtractorType.Miner ) )

    (
      getMinerProducts( data, modelItems, miners ),
      getOtherExtractionProducts( data, modelItems, otherExtractors )
    )
      .mapN: ( miner, other ) =>
        ( miner ++ other ).map:
          case ( gameItem, item, extractor, machine, tier ) =>
            (
              item,
              machine,
              ResourcePurity.cases
                .map( purity => ( purity, extractionRecipe( gameItem, item, extractor, purity, machine, tier ) ) )
            )

  def getMinerProducts(
      data: GameData,
      modelItems: ModelItems,
      converterExtractors: Vector[( Extractor, Machine, Tier )]
  ): ValidatedNel[String, Vector[( GameItem, Item, Extractor, Machine, Tier )]] =
    val ores: ValidatedNel[String, Vector[GameItem]] =
      data.items.values
        .collect:
          case item if item.nativeClass == NativeClass.resourceDescClass && item.form == GameForm.Solid => item
        .toVector
        .validNel

    ores
      .map( _.distinctBy( _.className ) )
      .andThen:
        _.traverse: item =>
          modelItems.get( item.className.translate ).tupleLeft( item )
      .map: oreItems =>
        for
          ( extractor, machine, tier ) <- converterExtractors
          ( gameItem, item )           <- oreItems
        yield ( gameItem, item, extractor, machine, tier )

  def getOtherExtractionProducts(
      data: GameData,
      modelItems: ModelItems,
      extractors: Vector[( Extractor, Machine, Tier )]
  ): ValidatedNel[String, Vector[( GameItem, Item, Extractor, Machine, Tier )]] =
    ( for
      ( extractor, machine, tier ) <- extractors
      allowedResources             <- extractor.allowedResources.toVector
      resource                     <- allowedResources.toList.toVector
    yield ( resource, extractor, machine, tier ) )
      .traverse:
        case ( resource, extractor, machine, tier ) =>
          ( data.getItem( resource ), modelItems.get( resource.translate ) ).mapN( ( _, _, extractor, machine, tier ) )

  def extractorMachine(
      classification: Map[ClassName[Extractor], Tier],
      extractor: Extractor
  ): ValidatedNel[String, ( Machine, Tier )] =
    (
      ExtractorType.cases
        .find( _.dataKey.fold( _ == extractor.extractorTypeName, _ == extractor.className ) )
        .toValidNel( s"No known extractor type for class ${extractor.className}, type ${extractor.extractorTypeName}" )
        .map( exType =>
          Machine(
            extractor.className.translate,
            extractor.displayName,
            MachineType( exType ),
            extractor.powerConsumption,
            extractor.powerConsumptionExponent
          )
        ),
      classification
        .get( extractor.className )
        .toValidNel( s"No known classification for class ${extractor.className}, type ${extractor.extractorTypeName}" )
    ).tupled

  def simpleProducerExtraction(
      modelItems: ModelItems,
      simpleProducer: SimpleProducer
  ): ValidatedNel[String, Option[( ( Item, Machine ), ExtractionRecipes )]] =
    val machineClass: ClassName[Machine] = simpleProducer.className.translate[Machine]
    SimpleProducer.knownSimpleProducers
      .get( simpleProducer.className )
      .traverse: itemClass =>
        (
          modelItems.get( itemClass.translate ),
          ExtractorType.cases
            .find( _.dataKey.fold( _ => false, _ == machineClass ) )
            .toValidNel( show"Extractor type not found for simple producer $machineClass" )
        )
          .mapN: ( item, extractorType ) =>
            val machine: Machine = Machine(
              machineClass,
              simpleProducer.displayName,
              MachineType( extractorType ),
              0d,
              1d
            )

            (
              ( item, machine ),
              ExtractionRecipes.Fixed(
                Recipe.Extraction(
                  ClassName( show"${itemClass}_$machineClass" ),
                  show"${item.displayName} (${simpleProducer.displayName})",
                  RecipeCategory.Extraction( Tier( 0 ) ),
                  Nil,
                  Countable( item, 1 ),
                  simpleProducer.timeToProduceItem,
                  machine,
                  Power.Fixed( 0d )
                )
              )
            )

  def extractionRecipe(
      gameItem: GameItem,
      item: Item,
      extractor: Extractor,
      purity: ResourcePurity,
      machine: Machine,
      tier: Tier
  ): Recipe.Extraction =
    Recipe.Extraction(
      ClassName( show"${item.className}_${purity.entryName.capitalize}_${extractor.className}" ),
      show"${item.displayName} ($purity, ${extractor.displayName})",
      RecipeCategory.Extraction( tier ),
      Nil,
      Countable( item, extractor.itemsPerCycle.toDouble / gameItem.form.simpleAmountFactor * purity.multiplier ),
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
  ): ValidatedNel[String, Option[Recipe.Manufacturing]] =
    NonEmptyList
      .fromList( recipe.producedIn.filter( data.manufacturers.keySet ) )
      .traverse( ms =>
        (
          classification
            .get( recipe.className )
            .toValidNel( show"Recipe ${recipe.displayName} [${recipe.className}] not classified" )
            .andThen( cat =>
              cat.manufacturing
                .toValidNel( show"Recipe ${recipe.displayName} [${recipe.className}] classified as $cat" )
            ),
          Option
            .when( ms.size == 1 )( ms.head )
            .toValidNel(
              show"Recipe ${recipe.displayName} [${recipe.className}] is produced in multiple manufacturers"
            )
            .andThen( validateManufacturer( data, _ ) ),
          validateRecipeItems( data, modelItems, recipe.ingredients ),
          validateRecipeItems( data, modelItems, recipe.products )
        ).mapN( ( cat, producer, ingredients, products ) =>
          Recipe.Manufacturing(
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

  def extractPowerRecipes(
      gameData: GameData,
      modelItems: ModelItems,
      classification: Map[ClassName[PowerGenerator], Tier]
  ): ValidatedNel[String, Vector[Recipe.PowerGeneration]] =
    gameData.powerGenerators.values.toVector.foldMapA: generator =>
      generator.fuels.traverse: fuel =>
        (
          modelItems.get( fuel.fuel.translate ),
          fuel.byproduct.traverse( _.traverse( cn => modelItems.get( cn.translate ) ) ),
          fuel.supplementalResource.traverse( cn => modelItems.get( cn.translate ) ),
          classification
            .get( generator.className )
            .toValidNel( s"No known classification for generator ${generator.displayName}" )
        ).mapN: ( f, bp, so, t ) =>
          val powerGenMW: Frac  = Frac.decimal( generator.powerProduction )
          val fuelValueMJ: Frac = Frac.decimal( f.fuelValue )

          val Frac( fAm, durMs ) = powerGenMW / ( 1000 *: fuelValueMJ )
          def sAm: Double        = fAm * f.fuelValue * generator.supplementalToPowerRatio / 1000

          Recipe.PowerGeneration(
            ClassName( s"${generator.className.name}__${f.className.name}" ),
            show"${f.displayName} in ${generator.displayName}",
            RecipeCategory.PowerGeneration( t ),
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
