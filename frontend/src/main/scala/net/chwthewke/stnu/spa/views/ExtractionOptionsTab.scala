package net.chwthewke.stnu
package spa
package views

import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import model.ClockSpeedPreset
import model.Item
import model.ResourceWeights
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.ExtractionOption
import spa.plan.ExtractionOptions
import spa.plan.PlanMsg

object ExtractionOptionsTab:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  private def minerSelectionField( env: Env, model: ExtractionOptions ): Html[ExtractionOption] =
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
    )

  private def clockSpeedField( model: ExtractionOptions ): Html[ExtractionOption] =
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

  private def extractorSelectionField( env: Env, model: ExtractionOptions ): Html[ExtractionOption] =
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
    )

  private def frackingSelectionField( env: Env, model: ExtractionOptions ): Html[ExtractionOption] =
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

  private def extractionSettingsBlock( env: Env, model: ExtractionOptions ): Html[ExtractionOption] =
    Html.div( b.panelBlock + b.columns )(
      Html.div( b.column + b.isHalf )(
        Html.label( b.label )( "Miner" ),
        minerSelectionField( env, model ),
        Html.label( b.label )( "Extractor clock speed" ),
        clockSpeedField( model )
      ),
      Html.div( b.column + b.isHalf )(
        Html.label( b.label )( "Extractor types" ),
        extractorSelectionField( env, model ),
        Html.label( b.label )( "Prefer fracking for" ),
        frackingSelectionField( env, model )
      )
    )

  private def resourceWeightsBlock( env: Env, model: ExtractionOptions ): Html[ExtractionOption] =
    Html.div( b.panelBlock )(
      Html.table( b.table )(
        Html.thead(
          Html.tr(
            Html.th( "Resource" ),
            Html.th( "Use less" ),
            Html.th( Html.style( CSS.`text-align`( "right" ) ) )( "Use more" ),
            Html.th()
          )
        ),
        Html.tbody(
          env.game.extractedItems.toList
            .sortBy( _.displayName )
            .map( resourceWeightRow( env, model ) )
        )
      )
    )

  private def resourceWeightRow( env: Env, model: ExtractionOptions )( item: Item ): Html[ExtractionOption] =
    val value: Int = model.resourceWeightSliders.getOrElse( item.className, ResourceWeights.range )
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
          Html.value  :=
            value.toString,
          Html.onChange( ExtractionOption.SetResourceWeight( item.className, _ ) )
        )
      ),
      Html.td( f"${value - ResourceWeights.range}%+d" )
    )

  def apply( env: Env, model: ExtractionOptions ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Extraction settings" ) ),
      extractionSettingsBlock( env, model ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Resource weights" ) ),
      resourceWeightsBlock( env, model )
    ).map( _.map( PlanMsg.SetExtractionOption( _ ) ) )
