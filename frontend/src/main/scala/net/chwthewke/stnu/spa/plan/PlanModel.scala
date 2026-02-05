package net.chwthewke.stnu
package spa
package plan

import cats.effect.Async
import cats.syntax.all.*
import tyrian.Cmd
import tyrian.Nav

import model.Recipe
import protocol.persistence.PlanId
import protocol.persistence.PlanName
import protocol.solver.SolverRequest
import spa.prod.ProdModel

class PlanModel(
    val env: Env,
    val ui: PlanModel.Ui,
    val name: PlanNameModel,
    val recipeOptions: RecipeOptionsInputModel,
    val resourceOptions: ResourceOptionsInputModel,
    val extractionOptions: ExtractionOptions,
    val logisticsOptions: LogisticsOptions,
    val powerOptions: PowerOptions,
    val requestSelection: RequestSelectionModel,
    val solution: Option[SolutionModel],
    val production: ProdModel
):
  import PlanModel.*

  def this(
      env: Env,
      ui: PlanModel.Ui,
      name: PlanNameModel,
      recipeOptions: RecipeOptionsInputModel,
      resourceOptions: ResourceOptionsInputModel,
      extractionOptions: ExtractionOptions,
      logisticsOptions: LogisticsOptions,
      powerOptions: PowerOptions,
      requestSelection: RequestSelectionModel,
      solution: Option[SolutionModel]
  ) =
    this(
      env,
      ui,
      name,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requestSelection,
      solution,
      PlanModel.productionOf( env, resourceOptions, extractionOptions, logisticsOptions, requestSelection, solution )
    )

  def copy(
      ui: PlanModel.Ui = this.ui,
      name: PlanNameModel = this.name,
      recipeOptions: RecipeOptionsInputModel = this.recipeOptions,
      resourceOptions: ResourceOptionsInputModel = this.resourceOptions,
      extractionOptions: ExtractionOptions = this.extractionOptions,
      logisticsOptions: LogisticsOptions = this.logisticsOptions,
      powerOptions: PowerOptions = this.powerOptions,
      requestSelection: RequestSelectionModel = this.requestSelection,
      solution: Option[SolutionModel] = this.solution
  ): PlanModel =
    val newProduction: ProdModel =
      if (
        ( resourceOptions ne this.resourceOptions ) ||
        ( extractionOptions ne this.extractionOptions ) ||
        ( logisticsOptions ne this.logisticsOptions ) ||
        ( requestSelection ne this.requestSelection ) ||
        ( solution ne this.solution )
      ) productionOf( env, resourceOptions, extractionOptions, logisticsOptions, requestSelection, solution )
      else production

    new PlanModel(
      env,
      ui,
      name,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requestSelection,
      solution,
      newProduction
    )

  def update[F[_]: Async]( http: Http[F], planMsg: PlanMsg ): ( PlanModel, Cmd[F, PlanMsg] ) = planMsg match
    case PlanMsg.PlanName( action ) =>
      val ( nextName, cmd ) = name.update( http, action )
      copy( name = nextName ) -> cmd
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
    case PlanMsg.ToggleProductionSummaryExpanded( open ) =>
      copy( ui = ui.setProductionSummaryExpanded( open ) ) -> Cmd.None
    case PlanMsg.ToggleProductionRowExpanded( recipe ) =>
      copy( ui = ui.toggleProductionRowExpanded( recipe ) ) -> Cmd.None
    case PlanMsg.SendSolverRequest =>
      this -> http
        .computeSolution( solverRequest )
        .map( PlanMsg.ReceiveSolverResponse( solverRequest, _ ) )
    case PlanMsg.ReceiveSolverResponse( solverRequest, solverResponse ) =>
      val computed: PlanModel = copy( solution = SolutionModel( solverRequest, solverResponse ).some )
      val ui: PlanModel.Ui    =
        computed.ui.resetProductionRowExpanded( computed.production.productionRows.map( _.recipe.className ) )
      computed.copy( ui = ui ) -> Cmd.None
    case PlanMsg.SaveRequest( confirm ) =>
      this -> http
        .save( save, confirm )
        .map( PlanNameAction.SaveResponse( _, save ) )
        .map( PlanMsg.PlanName( _ ) )
    case PlanMsg.PlanLoaded( _, None ) =>
      this -> Nav.pushUrl( locationWithPlanId( none ).toInternalLocation )
    case PlanMsg.PlanLoaded( id, Some( plan ) ) =>
      val ( model, cmd ) = PlanModel.load( env, ui, id, plan )
      model -> Cmd.Batch( Nav.pushUrl( locationWithPlanId( id.some ).toInternalLocation ), cmd )
    case PlanMsg.RevertPlan =>
      name.saved.fold( this -> Cmd.None ):
        case ( id, saved ) => PlanModel.load( env, ui, id, saved )
    case PlanMsg.ClearPlan =>
      PlanModel.init( env, ui ) -> Nav.pushUrl( locationWithPlanId( none ).toInternalLocation )

  def loadPlan[F[_]: Async]( http: Http[F], id: PlanId ): Cmd[F, PlanMsg] =
    if ( name.saved.exists( _._1 == id ) )
      Cmd.None
    else
      http.loadPlan( id ).map( PlanMsg.PlanLoaded( id, _ ) )

  def restore: PlanModel = copy(
    resourceOptions = if ( ui.optionsTab == OptionsTab.ResourceNodes ) resourceOptions.restore else resourceOptions,
    recipeOptions = if ( ui.optionsTab == OptionsTab.Recipes ) recipeOptions.restore else recipeOptions
  )

  def setOptionsTab( tab: Option[OptionsTab] ): PlanModel =
    copy( ui = ui.setOptionsTab( tab ) )

  def getLocation: LocationModel.Plan =
    LocationModel.Plan( ui.options, name.saved._1F )

  def locationToOpenOptions: LocationModel.Plan                   = getLocation.copy( options = ui.optionsTab.some )
  def locationToCloseOptions: LocationModel.Plan                  = getLocation.copy( options = none )
  def locationToOpenOption( tab: OptionsTab ): LocationModel.Plan = getLocation.copy( options = tab.some )
  def locationWithPlanId( planId: Option[PlanId] ): LocationModel.Plan = getLocation.copy( id = planId )

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

  private def solutionDirty( solution: SolutionModel ): Boolean =
    solution.requested != solverRequest

  lazy val canCompute: Boolean =
    if ( requestSelection.requestedAmounts.isEmpty )
      solution.exists( solutionDirty )
    else
      solution.forall( solutionDirty )

  val save: pp.Plan =
    pp.Plan(
      name.name,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requestSelection
    )

  val dirty: Boolean = name.saved.forall( _._2 != save )

object PlanModel:
  private def productionOf(
      env: Env,
      resourceOptions: ResourceOptionsInputModel,
      extractionOptions: ExtractionOptions,
      logisticsOptions: LogisticsOptions,
      requestSelection: RequestSelectionModel,
      solution: Option[SolutionModel]
  ): ProdModel =
    ProdModel(
      env,
      requestSelection,
      solution.map( s => ProdModel.Solution( env, s.requested, s.response ) ),
      resourceOptions.resourceNodes,
      extractionOptions,
      logisticsOptions.belts( env ),
      logisticsOptions.pipelines( env )
    )

  def init( env: Env, ui: Ui = Ui.init ): PlanModel =
    new PlanModel(
      env,
      ui,
      PlanNameModel.init( PlanName( "New plan" ), saved = None ),
      RecipeOptionsInputModel.init( env ),
      ResourceOptionsInputModel.init( env.game.defaultResourceOptions ),
      ExtractionOptions.init( env.game ),
      LogisticsOptions.init( env ),
      PowerOptions.init( env ),
      RequestSelectionModel.init,
      none
    )

  def load( env: Env, ui: Ui, planId: PlanId, saved: pp.Plan ): ( PlanModel, Cmd[Nothing, PlanMsg] ) =
    val model =
      new PlanModel(
        env = env,
        ui = ui,
        name = PlanNameModel.init( saved.name, saved = ( planId, saved ).some ),
        recipeOptions = RecipeOptionsInputModel.from( saved.recipeOptions ),
        resourceOptions = ResourceOptionsInputModel.from( env, saved.resourceOptions ),
        extractionOptions = ExtractionOptions.from( saved.extractionOptions ),
        logisticsOptions = LogisticsOptions.from( env, saved.logisticsOptions ),
        powerOptions = PowerOptions.from( saved.powerOptions ),
        requestSelection = RequestSelectionModel.from( saved.requestSelection ),
        solution = none
      )
    model -> ( if ( model.canCompute ) Cmd.Emit( PlanMsg.SendSolverRequest ) else Cmd.None )

  case class Ui(
      optionsOpen: Boolean,
      optionsTab: OptionsTab,
      requestSelectionVisible: Boolean,
      productionSummaryExpanded: Boolean,
      productionRowExpanded: Option[ClassName[Recipe]]
  ):
    def options: Option[OptionsTab] = Option.when( optionsOpen )( optionsTab )

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
      optionsOpen = false,
      optionsTab = OptionsTab.Recipes,
      requestSelectionVisible = false,
      productionSummaryExpanded = false,
      productionRowExpanded = none
    )
