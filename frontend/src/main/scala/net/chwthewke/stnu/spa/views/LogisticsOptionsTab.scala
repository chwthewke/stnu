package net.chwthewke.stnu
package spa
package views

import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.collection.immutable.SortedSet
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import model.Transport
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.LogisticsOption
import spa.plan.LogisticsOptions
import spa.plan.PlanMsg

object LogisticsOptionsTab:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  private def multipleTransportSelectionInput( model: LogisticsOptions )(
      allSelected: LogisticsOptions => SortedSet[ClassName[Transport]],
      selectOption: ( ClassName[Transport], Boolean ) => LogisticsOption,
      choice: Transport
  ): Html[LogisticsOption] =
    val isChecked: Boolean = allSelected( model ).contains( choice.className )
    Html
      .input(
        Html.`type` := "checkbox",
        Option.when[Attr[Nothing]]( isChecked )( Html.checked ),
        Html.onChange( _ => selectOption( choice.className, !isChecked ) )
      )
      .withKey( show"plan-options-logistics-select-${choice.className}".some )

  private def singleTransportSelectionInput( model: LogisticsOptions )(
      radiosName: String,
      singleSelected: LogisticsOptions => ClassName[Transport],
      setOption: ClassName[Transport] => LogisticsOption,
      choice: Transport
  ): Html[LogisticsOption] =
    Html
      .input(
        Html.`type` := "radio",
        Html.name   := radiosName,
        Option.when[Attr[Nothing]]( singleSelected( model ) == choice.className )( Html.checked ),
        Html.onChange( _ => setOption( choice.className ) )
      )
      .withKey( show"plan-options-logistics-set-${choice.className}".some )

  private def transportSelectionControl( env: Env, model: LogisticsOptions )(
      radiosName: String,
      allSelected: LogisticsOptions => SortedSet[ClassName[Transport]],
      selectOption: ( ClassName[Transport], Boolean ) => LogisticsOption,
      singleSelected: LogisticsOptions => ClassName[Transport],
      setOption: ClassName[Transport] => LogisticsOption,
      choice: Transport
  ): Html[LogisticsOption] =
    Html.div( b.control )(
      Html.label( if ( model.useAll ) b.checkbox else b.radio )(
        if ( model.useAll )
          multipleTransportSelectionInput( model )( allSelected, selectOption, choice )
        else
          singleTransportSelectionInput( model )( radiosName, singleSelected, setOption, choice ),
        nbsp,
        ( if ( model.useAll ) icon else icon.withStyles( CSS.marginLeft( "10px" ) ) )
          .verticalAlign()
          .withDropShadow()
          .transport( env, choice ),
        nbsp,
        Html.text( s"${choice.displayName} (${choice.perMinute} items/min)" )
      )
    )

  private def selectOneOrAll( env: Env, model: LogisticsOptions )(
      choices: NonEmptyList[Transport],
      radiosName: String,
      allSelected: LogisticsOptions => SortedSet[ClassName[Transport]],
      selectOption: ( ClassName[Transport], Boolean ) => LogisticsOption,
      singleSelected: LogisticsOptions => ClassName[Transport],
      setOption: ClassName[Transport] => LogisticsOption
  ): Html[LogisticsOption] =
    Html.div( b.field )(
      choices.toList
        .map(
          transportSelectionControl( env, model )( radiosName, allSelected, selectOption, singleSelected, setOption, _ )
        )
    )

  private def simpleLogisticsBlock( model: LogisticsOptions ): Html[LogisticsOption] =
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
    )

  private def conveyorBeltsBlock( env: Env, model: LogisticsOptions ): Html[LogisticsOption] =
    Html.div( b.panelBlock )(
      selectOneOrAll( env, model )(
        env.conveyorBelts,
        "logistics_prefs_belts",
        _.allBelts,
        LogisticsOption.SelectBelt( _, _ ),
        _.belt,
        LogisticsOption.SetBelt( _ )
      )
    )

  private def pipelinesBlock( env: Env, model: LogisticsOptions ): Html[LogisticsOption] =
    Html.div( b.panelBlock )(
      selectOneOrAll( env, model )(
        env.defaultPipelines,
        "logistics_prefs_pipelines",
        _.allPipelines,
        LogisticsOption.SelectPipeline( _, _ ),
        _.pipeline,
        LogisticsOption.SetPipeline( _ )
      )
    )

  def apply(
      env: Env,
      model: LogisticsOptions
  ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Simplified logistics" ) ),
      simpleLogisticsBlock( model ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Conveyor belts & lifts" ) ),
      conveyorBeltsBlock( env, model ),
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Pipelines" ) ),
      pipelinesBlock( env, model )
    ).map( _.map( PlanMsg.SetLogisticsOption( _ ) ) )
