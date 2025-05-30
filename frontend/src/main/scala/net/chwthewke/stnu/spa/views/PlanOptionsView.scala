package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import model.ExtractorType
import model.Item
import model.Recipe
import model.ResourceDistrib
import model.ResourcePurity
import model.ResourceWeights
import model.Tier
import model.Transport
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.ExtractionOption
import spa.plan.ExtractionOptions
import spa.plan.LogisticsOption
import spa.plan.LogisticsOptions
import spa.plan.OptionsTab
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.RecipeOption
import spa.plan.RecipeOptionsInputModel
import spa.plan.ResourceOptionsInputModel
import spa.prod.ClockSpeedPreset

object PlanOptionsView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def collapsedOptionsPanel( model: PlanModel ): Html[PlanMsg] =
    Html.div(
      Html.style( CSS.position( "absolute" ) )
    )(
      Html.a( b.button + b.isInfo, Html.href := LocationModel.Plan( model.ui.optionsTab.some ).toInternalLocation )(
        Html.text( "Options" ),
        nbsp,
        Html.i( p.regular.arrowsOut )()
      )
    )

  def optionsPanel( env: Env, model: PlanModel ): Html[PlanMsg] =
    Html.div( b.panel + b.mt2 + b.isInfo )(
      Html.div( b.panelHeading + b.p2, Html.style( CSS.display( "flex" ) ) )(
        Html.span( Html.style( CSS.flexGrow( "1" ) ) )( "Options" ),
        Html.a( b.hasTextInfoDark, Html.href := LocationModel.Plan( none ).toInternalLocation )(
          Html.i( p.regular.arrowsIn )()
        )
      ) ::
        Html.p( b.panelTabs )(
          OptionsTab.values.toList
            .map( option =>
              Html.a(
                Option.when[Attr[Nothing]]( option == model.ui.optionsTab )( b.isActive ),
                Html.href := LocationModel.Plan( option.some ).toInternalLocation
              )( option.description )
            )
        )
        :: Option
          .when( model.ui.optionsTab == OptionsTab.ResourceNodes )( resourceNodesTab( env, model.resourceOptions ) )
          .combineAll
        ++: Option
          .when( model.ui.optionsTab == OptionsTab.ResourcePrefs )( resourcePrefsTab( env, model.extractionOptions ) )
          .combineAll
        ++: Option
          .when( model.ui.optionsTab == OptionsTab.Logistics )( logisticsPrefsTab( env, model.logisticsOptions ) )
          .combineAll
        ++: Option
          .when( model.ui.optionsTab == OptionsTab.Recipes )( recipePrefsTab( env, model.recipeOptions ) )
          .combineAll
    )

  private def recipePrefsTab( env: Env, options: RecipeOptionsInputModel ): List[Html[PlanMsg]] =
    def tierButtons( message: Int => RecipeOption ): List[Html[RecipeOption]] =
      9.to( 0, -1 )
        .map: t =>
          Html.button( b.button + b.isSmall + b.isInfo, Html.onClick( message( t ) ) )( t.toString )
        .toList

    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Presets" ) ),
      Html.div( b.panelBlock )(
        Html.div( b.field )(
          Html.div( b.buttons + b.control )(
            Html.button(
              b.button + b.isSmall + b.isInfo,
              Html.title := "all recipes except matter conversion",
              Html.onClick( RecipeOption.Reset )
            )( "Reset to default" ),
            Html.button(
              b.button + b.isSmall + b.isInfo,
              Html.disabled
              // TODO
              //  ,Html.onClick()
            )( "Current recipes" ),
            Html.div( Html.style( CSS.flexGrow( "1" ) ) )(),
            Html.label(
              Html.input(
                b.checkbox,
                Html.`type` := "checkbox",
                Option.when[Attr[Nothing]]( options.hideFicsmas )( Html.checked ),
                Html.value := options.hideFicsmas.toString,
                Html.onChange( _ => RecipeOption.ToggleHideFicsmas( !options.hideFicsmas ) )
              ),
              Html.text( "Hide FICSMAS?" )
            )
          ),
          Html.div( b.buttons + b.control )(
            Html.text( "Max tier (alts)" ) :: tierButtons( t => RecipeOption.SetMaxTier( Tier( t ), withAlts = true ) )
          ),
          Html.div( b.buttons + b.control )(
            Html.text( "Max tier (no alts)" ) ::
              tierButtons( t => RecipeOption.SetMaxTier( Tier( t ), withAlts = false ) )
          )
        )
      ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Bulk toggles" ) ),
      Html.div( b.panelBlock )(
        Html.div( b.field + b.isHorizontal )(
          Html.div( b.buttons + b.control )(
            Html.text( "Alternates" ),
            Html.button(
              b.button + b.isSmall + b.isInfo,
              Html.onClick( RecipeOption.ToggleAlts( enable = true ) )
            )( "Add" ),
            Html.button(
              b.button + b.isSmall + b.isInfo,
              Html.onClick( RecipeOption.ToggleAlts( enable = false ) )
            )( "Remove" ),
            Html.div( Html.style( CSS.flexGrow( "1" ) ) )(),
            Html.text( "Matter conversion" ),
            Html.button(
              b.button + b.isSmall + b.isInfo,
              Html.onClick( RecipeOption.ToggleConversion( enable = true ) )
            )( "Add" ),
            Html.button(
              b.button + b.isSmall + b.isInfo,
              Html.onClick( RecipeOption.ToggleConversion( enable = false ) )
            )( "Remove" )
          )
        )
      ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Recipe list" ) ),
      Html.div( b.panelBlock, Html.styles( CSS.flexDirection( "column" ), CSS.alignItems( "stretch" ) ) ):
        WordsSearch( options.search, RecipeOption.SearchInput( _ ), RecipeOption.SearchReset ) ::
          env.game.manufacturingRecipes
            .foldMap: recipe =>
              SortedMap( recipe.products.head.item -> NonEmptySet.one( recipe ) )
            .toList
            .mapFilter:
              case ( item, recipes ) =>
                val itemMatch: Boolean = options.search.terms.matches( Vector( item.displayName ) )
                val matchingRecipes: SortedSet[Recipe.Manufacturing] = recipes.filter: recipe =>
                  itemMatch || options.search.terms.matches( Vector( recipe.displayName ) )

                Option.when( matchingRecipes.nonEmpty )( ( item, matchingRecipes ) )
            .map:
              case ( item, recipes ) =>
                Html.div( b.field )(
                  Html.label( b.label )(
                    icon.verticalAlign().withDropShadow().item( env, item ),
                    nbsp,
                    Html.text( item.displayName )
                  ) ::
                    recipes.toList.map: recipe =>
                      val isChecked: Boolean = options.allowedRecipes.contains( recipe.className )
                      Html.div( b.control )(
                        Html.label( b.checkbox )(
                          Html.input(
                            Html.`type` := "checkbox",
                            Html.name   := s"res_prefs_recipe_${recipe.className}",
                            Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
                            Html.value := isChecked.toString,
                            Html.onChange( _ => RecipeOption.SetRecipe( recipe.className, !isChecked ) )
                          ),
                          Html.span( Html.title := recipe.describe )( recipe.displayName )
                        )
                      )
                )
    ).map( _.map( PlanMsg.SetRecipeOption( _ ) ) )

  private def resourceNodesTab( env: Env, model: ResourceOptionsInputModel ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Resource nodes" ) ),
      Html.div( b.panelBlock )(
        Html.table( b.table + b.isResponsive )(
          Html.thead(
            Html.tr(
              Html.th( "Item" ),
              Html.th( Html.colspan := "2" )( "Impure" ),
              Html.th( Html.colspan := "2" )( "Normal" ),
              Html.th( Html.colspan := "2" )( "Pure" )
            )
          ),
          Html.tbody(
            env.game.defaultResourceOptions.resourceNodes.toList
              .filter:
                case ( ExtractorType.Miner | ExtractorType.OilPump | ExtractorType.Fracking, _ ) => true
                case _                                                                           => false
              .sortBy( _._1 )
              .flatMap:
                case ( extractor, resources ) =>
                  Html.tr( Html.td( Html.colspan := "7" )( Html.strong( extractor.description ) ) ) ::
                    resources.toList
                      .mapFilter:
                        case ( itemClass, distrib ) => env.game.items.get( itemClass ).tupleRight( distrib )
                      .map:
                        case ( item, distrib ) =>
                          Html.tr(
                            Html.td( Html.style( CSS.verticalAlign( "middle" ) ) )(
                              icon.verticalAlign().withDropShadow().item( env, item ),
                              nbsp,
                              Html.text( item.displayName )
                            ) ::
                              resourceNodeInputs( extractor, item.className, distrib, model )
                          )
          )
        )
      )
    )

  private def resourceNodeInputs(
      extractor: ExtractorType,
      item: ClassName[Item],
      max: ResourceDistrib,
      current: ResourceOptionsInputModel
  ): List[Html[PlanMsg]] =
    ResourcePurity.cases.toList
      .map: purity =>
        ( purity, max.get( purity ), current.inputs.get( ( extractor, item, purity ) ).flatMap( _.output ) )
      .flatMap:
        case ( purity, max, current ) =>
          List(
            Html.td(
              Html.input(
                Html.`type` := "number",
                Html.min    := "0",
                Html.max    := max.toString,
                current.map( v => Html.value := v ),
                Html.style( CSS.width( "3em" ) ),
                Html.onInput( PlanMsg.SetResourceDistribution( extractor, item, purity, _ ) )
              )
            ),
            Html.td( s"($max)" )
          )

  private def resourcePrefsTab( env: Env, model: ExtractionOptions ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Extraction settings" ) ),
      Html.div( b.panelBlock + b.columns )(
        Html.div( b.column + b.isHalf )(
          Html.label( b.label )( "Miner" ),
          Html.div( b.field )(
            env.miners
              .map( m =>
                Html.div( b.control )(
                  Html.label( b.radio )(
                    Html.input(
                      Html.`type` := "radio",
                      Html.name   := "res_prefs_miner",
                      Option.when[Attr[Nothing]]( model.minerClass == m.className )( Html.checked ),
                      Html.onChange( _ => ExtractionOption.SetMiner( m.className ) )
                    ),
                    nbsp,
                    icon.verticalAlign().withDropShadow().machine( env, m ),
                    nbsp,
                    Html.text( m.displayName )
                  )
                )
              )
          ),
          Html.label( b.label )( "Extractor clock speed" ),
          Html.div( b.field )(
            ClockSpeedPreset.values.toList.map: cs =>
              Html.div( b.control )(
                Html.label( b.radio )(
                  Html.input(
                    Html.`type` := "radio",
                    Html.name   := "res_prefs_clock_speed",
                    Option.when[Attr[Nothing]]( model.clockSpeed == cs )( Html.checked ),
                    Html.onChange( _ => ExtractionOption.SetClockSpeed( cs ) )
                  ),
                  nbsp,
                  Html.text( cs.toString )
                )
              )
          )
        ),
        Html.div( b.column + b.isHalf )(
          Html.label( b.label )( "Extractor types" ),
          Html.div( b.field )(
            env.machinesByExtractorType.toList.map:
              case ( extractor, machine ) =>
                val isChecked = model.extractors.contains( extractor )
                Html.div( b.control )(
                  Html.label( b.checkbox )(
                    Html.input(
                      Html.`type` := "checkbox",
                      Html.name   := s"res_prefs_ex_$extractor",
                      Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
                      Html.value := isChecked.toString,
                      Html.onChange( ExtractionOption.ToggleExtractorType( extractor, _ ) )
                    ),
                    icon.verticalAlign().withDropShadow().machine( env, machine ),
                    nbsp,
                    Html.text( extractor.description )
                  )
                )
          ),
          Html.label( b.label )( "Prefer fracking for" ),
          Html.div( b.field )(
            env.itemsExtractibleByFrackingAndOtherMethod.map: item =>
              val isChecked: Boolean = model.preferFracking.contains( item.className )
              Html.div( b.control )(
                Html.label( b.checkbox )(
                  Html.input(
                    Html.`type` := "checkbox",
                    Html.name   := s"res_prefs_frack_${item.className}",
                    Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
                    Html.value := isChecked.toString,
                    Html.onChange( ExtractionOption.ToggleFrackingPreference( item.className, _ ) )
                  ),
                  icon.verticalAlign().withDropShadow().item( env, item ),
                  nbsp,
                  Html.text( item.displayName )
                )
              )
          )
        )
      ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Resource weights" ) ),
      Html.div( b.panelBlock )(
        Html.table( b.table )(
          Html.thead(
            Html.tr(
              Html.th( "Resource" ),
              Html.th( "Use less" ),
              Html.th( Html.style( CSS.`text-align`( "right" ) ) )( "Use more" )
            )
          ),
          Html.tbody(
            env.game.extractedItems.toList
              .sortBy( _.displayName )
              .map: item =>
                Html.tr(
                  Html.td(
                    icon.verticalAlign().withDropShadow().item( env, item ),
                    nbsp,
                    Html.text( item.displayName )
                  ),
                  Html.td( Html.colspan := "2" )(
                    Html.input(
                      Html.`type` := "range",
                      Html.min    := "0",
                      Html.max    := ( 2 * ResourceWeights.range ).toString,
                      Html.value  := ResourceWeights.range.toString,
                      Html.onChange( ExtractionOption.SetResourceWeight( item.className, _ ) )
                    )
                  )
                )
          )
        )
      )
    ).map( _.map( PlanMsg.SetExtractionOption( _ ) ) )

  private def selectOneOrAll(
      model: LogisticsOptions,
      env: Env,
      choices: NonEmptyList[Transport],
      radiosName: String,
      allSelected: LogisticsOptions => SortedSet[ClassName[Transport]],
      selectOption: ( ClassName[Transport], Boolean ) => LogisticsOption,
      singleSelected: LogisticsOptions => ClassName[Transport],
      setOption: ClassName[Transport] => LogisticsOption
  ): Html[LogisticsOption] =
    Html.div( b.field )(
      choices.toList
        .map: choice =>
          Html.div( b.control )(
            Html.label( if ( model.useAll ) b.checkbox else b.radio )(
              if model.useAll then
                val isChecked = allSelected( model ).contains( choice.className )
                Html
                  .input(
                    Html.`type` := "checkbox",
                    Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
                    Html.onChange( _ => selectOption( choice.className, !isChecked ) )
                  )
                  .withKey( show"plan-options-logistics-select-${choice.className}".some )
              else
                Html
                  .input(
                    Html.`type` := "radio",
                    Html.name   := radiosName,
                    Option.when[Attr[Nothing]]( singleSelected( model ) == choice.className )( Html.checked ),
                    Html.onChange( _ => setOption( choice.className ) )
                  )
                  .withKey( show"plan-options-logistics-set-${choice.className}".some )
              ,
              nbsp,
              ( if ( model.useAll ) icon else icon.withStyles( CSS.marginLeft( "10px" ) ) )
                .verticalAlign()
                .withDropShadow()
                .transport( env, choice ),
              nbsp,
              Html.text( s"${choice.displayName} (${choice.perMinute} items/min)" )
            )
          )
    )

  private def logisticsPrefsTab(
      env: Env,
      model: LogisticsOptions
  ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Simplified logistics" ) ),
      Html.div( b.panelBlock )(
        Html.div( b.field )(
          Html.div( b.control )(
            Html.label( b.checkbox )(
              Html.input(
                Html.`type` := "checkbox",
                Option.when[Attr[Nothing]]( !model.useAll )( Html.checked ),
                Html.onChange( _ => LogisticsOption.ToggleUseAll( !model.useAll ) )
              ),
              Html.text( "Use a single tier of belt and pipeline" )
            )
          )
        )
      ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Conveyor belts & lifts" ) ),
      Html.div( b.panelBlock )(
        selectOneOrAll(
          model,
          env,
          env.conveyorBelts,
          "logistics_prefs_belts",
          _.allBelts,
          LogisticsOption.SelectBelt( _, _ ),
          _.belt,
          LogisticsOption.SetBelt( _ )
        )
      ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Pipelines" ) ),
      Html.div( b.panelBlock )(
        selectOneOrAll(
          model,
          env,
          env.defaultPipelines,
          "logistics_prefs_pipelines",
          _.allPipelines,
          LogisticsOption.SelectPipeline( _, _ ),
          _.pipeline,
          LogisticsOption.SetPipeline( _ )
        )
      )
    ).map( _.map( PlanMsg.SetLogisticsOption( _ ) ) )
