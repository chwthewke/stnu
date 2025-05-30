package net.chwthewke.stnu
package spa
package plan

import cats.effect.Async
import cats.syntax.all.*
import tyrian.Cmd

import model.Recipe
import protocol.solver.SolverRequest
import spa.prod.ProdModel

case class PlanModel(
    env: Env,
    ui: PlanModel.Ui,
    recipeOptions: RecipeOptionsInputModel,
    resourceOptions: ResourceOptionsInputModel,
    extractionOptions: ExtractionOptions,
    logisticsOptions: LogisticsOptions,
    powerOptions: PowerOptions,
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
    case PlanMsg.SetPowerOption( powerOption ) =>
      copy( powerOptions = powerOptions.setOption( env, powerOption ) ) -> Cmd.None
    case PlanMsg.RequestSelection( action ) =>
      val ( newRequestSelection, cmd ) = requestSelection.update[F]( action )
      copy( requestSelection = newRequestSelection ) -> cmd
    case PlanMsg.ToggleRequestSelection( enable ) =>
      copy( ui = ui.copy( requestSelectionVisible = enable ) ) -> Cmd.None
    case PlanMsg.SendSolverRequest =>
      this -> http
        .computeSolution( solverRequest )
        .map( PlanMsg.ReceiveSolverResponse( solverRequest, _ ) )
    case PlanMsg.ReceiveSolverResponse( solverRequest, solverResponse ) =>
      val computed: PlanModel = copy( solution = SolutionModel( solverRequest, solverResponse ).some )
      val ui: PlanModel.Ui    =
        computed.ui.resetProductionRowExpanded( computed.production.rows.map( _.recipe.className ) )
      computed.copy( ui = ui ) -> Cmd.None
    case PlanMsg.ToggleProductionSummaryExpanded( open ) =>
      copy( ui = ui.setProductionSummaryExpanded( open ) ) -> Cmd.None
    case PlanMsg.ToggleProductionRowExpanded( recipe ) =>
      copy( ui = ui.toggleProductionRowExpanded( recipe ) ) -> Cmd.None

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
      recipeOptions.allowedRecipes ++
        env.game.powerRecipes
          .filter( rec => rec.products.nonEmpty && powerOptions.allowedGenerators.contains( rec.producedIn.className ) )
          .map( _.className ),
      extractionOptions.resources( env, resourceOptions.resourceNodes )
    )

  lazy val production: ProdModel =
    ProdModel(
      env,
      requestSelection,
      solution.map( s => ProdModel.Solution( env, s.requested, s.response ) ),
      resourceOptions.resourceNodes,
      extractionOptions,
      logisticsOptions.belts( env ),
      logisticsOptions.pipelines( env ),
      ui.productionRowExpanded
    )

  private def solutionDirty( solution: SolutionModel ): Boolean =
    solution.requested != solverRequest

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
      PowerOptions.init( env ),
      RequestSelectionModel.init,
      none
    )

  case class Ui(
      optionsOpen: Boolean,
      optionsTab: OptionsTab,
      requestSelectionVisible: Boolean,
      productionSummaryExpanded: Boolean,
      productionRowExpanded: Option[ClassName[Recipe]]
  ):
    def setOptionsTab( optionsTab: Option[OptionsTab] ): Ui =
      copy(
        optionsOpen = optionsTab.isDefined,
        optionsTab = optionsTab.getOrElse( this.optionsTab )
      )
    def setProductionSummaryExpanded( isOpen: Boolean ): Ui =
      copy( productionSummaryExpanded = isOpen )

    def toggleProductionRowExpanded( recipe: ClassName[Recipe] ): Ui =
      copy(productionRowExpanded =
        if ( productionRowExpanded.contains_( recipe ) ) none
        else recipe.some
      )

    def resetProductionRowExpanded( recipes: List[ClassName[Recipe]] ): Ui =
      copy( productionRowExpanded = productionRowExpanded.filter( recipes.contains_ ) )

  object Ui:
    val init: Ui = Ui(
      optionsOpen = true,
      optionsTab = OptionsTab.Recipes,
      requestSelectionVisible = false,
      productionSummaryExpanded = false,
      productionRowExpanded = none
    )
