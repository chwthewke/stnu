package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*
import tyrian.Cmd

case class PlanModel(
    env: Env,
    ui: PlanModel.Ui,
    recipeOptions: RecipeOptionsInputModel,
    resourceOptions: ResourceOptionsInputModel,
    extractionOptions: ExtractionOptions,
    logisticsOptions: LogisticsOptions
):
  def update[F[_]]( env: Env, planMsg: PlanMsg ): ( PlanModel, Cmd[F, PlanMsg] ) = planMsg match
    case PlanMsg.SetResourceDistribution( extractor, item, purity, value ) =>
      copy( resourceOptions = resourceOptions.setResourceDistribution( extractor, item, purity, value ) ) -> Cmd.None
    case PlanMsg.SetExtractionOption( extractionOption ) =>
      copy( extractionOptions = extractionOptions.setOption( extractionOption ) ) -> Cmd.None
    case PlanMsg.SetLogisticsOption( logisticsOption ) =>
      copy( logisticsOptions = logisticsOptions.setOption( env, logisticsOption ) ) -> Cmd.None
    case PlanMsg.SetRecipeOption( recipeOption ) =>
      copy( recipeOptions = recipeOptions.setOption( env, recipeOption ) ) -> Cmd.None

  def restore: PlanModel = copy(
    resourceOptions = if ( ui.optionsTab == OptionsTab.ResourceNodes ) resourceOptions.restore else resourceOptions,
    recipeOptions = if ( ui.optionsTab == OptionsTab.Recipes ) recipeOptions.restore else recipeOptions
  )

  def setOptionsTab( tab: Option[OptionsTab] ): PlanModel =
    copy( ui = ui.setOptionsTab( tab ) )

  def getLocation: LocationModel.Plan =
    LocationModel.Plan( Option.when( ui.optionsOpen )( ui.optionsTab ) )

object PlanModel:
  def init( env: Env, optionsTab: Option[OptionsTab] ): PlanModel =
    PlanModel(
      env,
      Ui.init.setOptionsTab( optionsTab ),
      RecipeOptionsInputModel.init( env ),
      ResourceOptionsInputModel.init( env.game.defaultResourceOptions ),
      ExtractionOptions.init( env.game ),
      LogisticsOptions.init( env )
    )

  case class Ui(
      optionsOpen: Boolean,
      optionsTab: OptionsTab
  ):
    def setOptionsTab( optionsTab: Option[OptionsTab] ): Ui =
      copy(
        optionsOpen = optionsTab.isDefined,
        optionsTab = optionsTab.getOrElse( this.optionsTab )
      )

  object Ui:
    val init: Ui = Ui( optionsOpen = true, optionsTab = OptionsTab.Recipes )
