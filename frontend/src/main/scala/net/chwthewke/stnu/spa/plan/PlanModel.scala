package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*
import tyrian.Cmd

import model.Model

case class PlanModel(
    ui: PlanModel.Ui,
    resourceOptions: ResourceOptionsInputModel,
    extractionOptions: ExtractionOptionsInputModel
):
  def update[F[_]]( planMsg: PlanMsg ): ( PlanModel, Cmd[F, PlanMsg] ) = planMsg match
    case PlanMsg.SetOptionsTab( option ) =>
      copy(
        ui = ui.setOptionsTab( option ),
        resourceOptions = if ( option == OptionsTab.ResourceNodes ) resourceOptions.restore else resourceOptions
      ) -> Cmd.None
    case PlanMsg.SetResourceDistribution( extractor, item, purity, value ) =>
      copy( resourceOptions = resourceOptions.setResourceDistribution( extractor, item, purity, value ) ) -> Cmd.None
    case PlanMsg.SetExtractionOption(extractionOption) =>
      copy(extractionOptions = extractionOptions.setOption(extractionOption)) -> Cmd.None
  def restore: PlanModel = copy( resourceOptions = resourceOptions.restore )

object PlanModel:
  def init( model: Model ): PlanModel = {
    PlanModel(
      Ui.init,
      ResourceOptionsInputModel.init( model.defaultResourceOptions ),
      ExtractionOptionsInputModel.init( model )
    )
  }

  case class Ui(
      optionsOpen: Boolean,
      optionsTab: OptionsTab
  ):
    def setOptionsTab( optionsTab: OptionsTab ): Ui = copy( optionsTab = optionsTab )

  object Ui:
    val init: Ui = Ui( optionsOpen = true, optionsTab = OptionsTab.ResourcePrefs )
