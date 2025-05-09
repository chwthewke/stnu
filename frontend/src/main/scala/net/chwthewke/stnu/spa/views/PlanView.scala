package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import model.ExtractorType
import model.Item
import model.Model
import model.ResourceDistrib
import model.ResourcePurity
import net.chwthewke.stnu.spa.plan.ExtractionOption
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.ExtractionOptionsInputModel
import spa.plan.OptionsTab
import spa.plan.PlanModel
import spa.plan.PlanMsg
import spa.plan.ResourceOptionsInputModel
import spa.prod.ClockSpeed

object PlanView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: PlanModel ): Html[PlanMsg] =
    Html.div( b.columns )(
      Html.div( b.column + b.isOneQuarter )(
        optionsPanel( env, model )
      )
    )

  private def optionsPanel( env: Env, model: PlanModel ): Html[PlanMsg] =
    Html.div( b.panel + b.mt2 + b.isInfo )(
      Html.div( b.panelHeading + b.p2, Html.style( CSS.display( "flex" ) ) )(
        Html.span( Html.style( CSS.flexGrow( "1" ) ) )( "Options" ),
        Html.i( p.regular.arrowsIn )()
      ) ::
        Html.p( b.panelTabs )(
          OptionsTab.values.toList
            .map( option =>
              Html.a(
                Option.when[Attr[Nothing]]( option == model.ui.optionsTab )( b.isActive ),
                Option.when( option != model.ui.optionsTab )( Html.onClick( PlanMsg.SetOptionsTab( option ) ) )
              )( option.description )
            )
        )
        :: Option
          .when( model.ui.optionsTab == OptionsTab.ResourceNodes )( resourceNodesTab( env, model.resourceOptions ) )
          .combineAll
        ++: Option
          .when( model.ui.optionsTab == OptionsTab.ResourcePrefs )( resourcePrefsTab( env, model.extractionOptions ) )
          .combineAll
    )

  private def resourceNodesTab( env: Env, model: ResourceOptionsInputModel ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Resource nodes" ) ),
      Html.div( b.panelBlock )(
        Html.table( b.table + b.isResponsive /* no borders */ )(
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
                      .mapFilter {
                        case ( itemClass, distrib ) => env.game.items.get( itemClass ).tupleRight( distrib )
                      }
                      .map:
                        case ( item, distrib ) =>
                          Html.tr(
                            Html.td( Icons.item( env, item ) ) ::
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

  private def resourcePrefsTab( env: Env, model: ExtractionOptionsInputModel ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Extraction settings" ) ),
      Html.div( b.panelBlock + b.columns )(
        Html.div( b.column + b.isHalf )(
          Html.label( b.label )( "Miner" ),
          Html.div( b.field )(
            env.game.machines.values
              .filter: m =>
                m.machineType.extractor.contains( ExtractorType.Miner )
              .toList
              .sortBy( _.powerConsumption )
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
                    Html.text( m.displayName )
                  )
                )
              )
          ),
          Html.label( b.label )( "Extractor clock speed" ),
          Html.div( b.field )(
            ClockSpeed.values.toList.map: cs =>
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
            ExtractorType.cases.toList.map: extractor =>
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
                  nbsp,
                  Html.text( extractor.description )
                )
              )
          ),
          Html.label( b.label )( "Prefer fracking for" ),
          Html.div( b.field )(
            itemsExtractibleByFrackingAndOtherMethod( env.game ).map: item =>
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
                    Icons.item( env, item ),
                    Html.span( b.pl2 )( item.displayName )
                  ),
                  Html.td( Html.colspan := "2" )(
                    Html.input(
                      Html.`type` := "range",
                      Html.min    := "0",
                      Html.max    := "8",
                      Html.value  := "4",
                      Html.onChange( ExtractionOption.SetResourceWeight( item.className, _ ) )
                    )
                  )
                )
          )
        )
      )
    ).map( _.map( PlanMsg.SetExtractionOption( _ ) ) )

  // TODO move computations like this (akward to call on every view refresh)
  //   into PlanModel.init (store in Ui perhaps)
  private def itemsExtractibleByFrackingAndOtherMethod( game: Model ): List[Item] =
    val extractorTypesByItem: Map[ClassName[Item], Set[ExtractorType]] =
      game.defaultResourceOptions.resourceNodes
        .fmap( _.keys )
        .toVector
        .foldMap:
          case ( extractor, items ) =>
            items.toVector.foldMap( item => Map( item -> Set( extractor ) ) )
    extractorTypesByItem
      .filter:
        case ( _, extractors ) => extractors.size > 1 && extractors.contains( ExtractorType.Fracking )
      .keys
      .toList
      .mapFilter( game.items.get )
      .sortBy( _.displayName )
