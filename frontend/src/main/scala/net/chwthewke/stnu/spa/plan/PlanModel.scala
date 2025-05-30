package net.chwthewke.stnu
package spa
package plan

import cats.effect.Async
import cats.syntax.all.*
import tyrian.Cmd

import protocol.solver.SolverRequest

case class PlanModel(
    env: Env,
    ui: PlanModel.Ui,
    recipeOptions: RecipeOptionsInputModel,
    resourceOptions: ResourceOptionsInputModel,
    extractionOptions: ExtractionOptions,
    logisticsOptions: LogisticsOptions,
    requestSelection: RequestSelectionModel,
    solution: Option[SolutionModel]
):
  def update[F[_]: Async]( http: Http[F], planMsg: PlanMsg ): ( PlanModel, Cmd[F, PlanMsg] ) = planMsg match
    case PlanMsg.SetResourceDistribution( extractor, item, purity, value ) =>
      copy( resourceOptions = resourceOptions.setResourceDistribution( extractor, item, purity, value ) ) -> Cmd.None
    case PlanMsg.SetExtractionOption( extractionOption ) =>
      copy( extractionOptions = extractionOptions.setOption( extractionOption ) ) -> Cmd.None
    case PlanMsg.SetLogisticsOption( logisticsOption ) =>
      copy( logisticsOptions = logisticsOptions.setOption( env, logisticsOption ) ) -> Cmd.None
    case PlanMsg.SetRecipeOption( recipeOption ) =>
      copy( recipeOptions = recipeOptions.setOption( env, recipeOption ) ) -> Cmd.None
    case PlanMsg.RequestSelection( action ) =>
      val ( newRequestSelection, cmd ) = requestSelection.update[F]( action )
      copy( requestSelection = newRequestSelection ) -> cmd
    case PlanMsg.ToggleRequestSelection( enable ) =>
      copy( ui = ui.copy( requestSelectionVisible = enable ) ) -> Cmd.None
    case PlanMsg.SendSolverRequest =>
      this -> http
        .computeSolution( solverRequest )
        .map( PlanMsg.ReceiveSolverResponse( solverRequest, otherInputs, _ ) )
    case PlanMsg.ReceiveSolverResponse( solverRequest, otherInputs, solverResponse ) =>
      copy( solution = SolutionModel( solverRequest, otherInputs, solverResponse ).some ) -> Cmd.None

  def restore: PlanModel = copy(
    resourceOptions = if ( ui.optionsTab == OptionsTab.ResourceNodes ) resourceOptions.restore else resourceOptions,
    recipeOptions = if ( ui.optionsTab == OptionsTab.Recipes ) recipeOptions.restore else recipeOptions
  )

  def setOptionsTab( tab: Option[OptionsTab] ): PlanModel =
    copy( ui = ui.setOptionsTab( tab ) )

  def getLocation: LocationModel.Plan =
    LocationModel.Plan( Option.when( ui.optionsOpen )( ui.optionsTab ) )

  lazy val solverRequest: SolverRequest =
    SolverRequest(
      env.game.version.version,
      requestSelection.requested,
      recipeOptions.allowedRecipes ++ env.game.powerRecipes.filter( _.products.nonEmpty ).map( _.className ), // FIXME select power gen
      extractionOptions.resources( env, resourceOptions )
    )

  lazy val otherInputs: SolutionModel.OtherInputs =
    SolutionModel.OtherInputs(
      extractionOptions.preferFracking,
      logisticsOptions.belts( env ),
      logisticsOptions.pipelines( env )
    )

  private def solutionDirty( solution: SolutionModel ): Boolean =
    solution.requested != solverRequest || solution.otherInputs != otherInputs

  lazy val canCompute: Boolean =
    if ( requestSelection.requestedAmounts.isEmpty )
      solution.exists( solutionDirty )
    else
      solution.forall( solutionDirty )

object PlanModel:
  def init( env: Env, optionsTab: Option[OptionsTab] ): PlanModel =
    PlanModel(
      env,
      Ui.init.setOptionsTab( optionsTab ),
      RecipeOptionsInputModel.init( env ),
      ResourceOptionsInputModel.init( env.game.defaultResourceOptions ),
      ExtractionOptions.init( env.game ),
      LogisticsOptions.init( env ),
      RequestSelectionModel.init,
      none
    )

  case class Ui(
      optionsOpen: Boolean,
      optionsTab: OptionsTab,
      requestSelectionVisible: Boolean
  ):
    def setOptionsTab( optionsTab: Option[OptionsTab] ): Ui =
      copy(
        optionsOpen = optionsTab.isDefined,
        optionsTab = optionsTab.getOrElse( this.optionsTab )
      )

  object Ui:
    val init: Ui = Ui( optionsOpen = true, optionsTab = OptionsTab.Recipes, requestSelectionVisible = false )
