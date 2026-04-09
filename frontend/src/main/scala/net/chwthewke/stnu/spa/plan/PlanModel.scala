package net.chwthewke.stnu
package spa
package plan

import cats.effect.Async
import cats.syntax.all.*
import tyrian.Cmd
import tyrian.Nav

import protocol.persistence.PlanId
import protocol.persistence.PlanName
import protocol.solver.SolverRequest
import spa.prod.Flows
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
    val requests: RequestsModel,
    val solution: Option[SolutionModel],
    val production: ProdModel,
    val productionUi: ProdModel.Ui,
    val flows: Either[pp.Flows, Flows]
):
  import PlanModel.*

  def copy(
      ui: PlanModel.Ui = this.ui,
      name: PlanNameModel = this.name,
      recipeOptions: RecipeOptionsInputModel = this.recipeOptions,
      resourceOptions: ResourceOptionsInputModel = this.resourceOptions,
      extractionOptions: ExtractionOptions = this.extractionOptions,
      logisticsOptions: LogisticsOptions = this.logisticsOptions,
      powerOptions: PowerOptions = this.powerOptions,
      requests: RequestsModel = this.requests,
      solution: Option[SolutionModel] = this.solution,
      productionUi: ProdModel.Ui = this.productionUi,
      flows: Either[pp.Flows, Flows] = this.flows
  ): PlanModel =
    val updateProduction =
      ( resourceOptions ne this.resourceOptions ) ||
        ( extractionOptions ne this.extractionOptions ) ||
        ( logisticsOptions ne this.logisticsOptions ) ||
        ( requests ne this.requests ) ||
        ( solution ne this.solution )
    val newProduction: ProdModel =
      if ( updateProduction )
        productionOf( env, resourceOptions, extractionOptions, logisticsOptions, requests, solution )
      else production

    val ( newFlows: Either[pp.Flows, Flows], newProductionUi: ProdModel.Ui ) =
      if ( updateProduction )
        invalidateProduction( newProduction )( flows, productionUi )
      else
        (
          flows,
          if ( flows ne this.flows )
            flows.toOption.foldLeft( productionUi )( _.setFlows( _ ) )
          else productionUi
        )

    new PlanModel(
      env,
      ui,
      name,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requests,
      solution,
      newProduction,
      newProductionUi,
      newFlows
    )

  private def restoreFlows( prod: ProdModel, flows: Either[pp.Flows, Flows] ): Either[pp.Flows, Flows] =
    if ( prod.solution.isEmpty ) flows.flatMap( Left( _ ) )
    else flows.fold( Flows.from( prod, _ ), Flows.from( prod, _ ) ).asRight

  private def invalidateProduction(
      newProduction: ProdModel
  )( flows: Either[pp.Flows, Flows], productionUi: ProdModel.Ui ): ( Either[pp.Flows, Flows], ProdModel.Ui ) =
    val newFlows: Either[pp.Flows, Flows] = restoreFlows( newProduction, flows )
    val newProductionUi: ProdModel.Ui     = newFlows.fold( _ => productionUi, productionUi.setFlows )
    ( newFlows, newProductionUi )

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
      copy( recipeOptions = recipeOptions.setOption( env, recipeOption, solution ) ) -> Cmd.None
    case PlanMsg.SetPowerOption( powerOption ) =>
      copy( powerOptions = powerOptions.setOption( env, powerOption ) ) -> Cmd.None
    case PlanMsg.Requests( action ) =>
      copy( requests = requests.update( action ) ) -> Cmd.None
    case PlanMsg.ToggleRequestSelection( enable ) =>
      copy( ui = ui.copy( requestSelectionVisible = enable ) ) -> Cmd.None
    case PlanMsg.ToggleProductionSummaryExpanded( open ) =>
      copy( productionUi = productionUi.setProductionSummaryExpanded( open ) ) -> Cmd.None
    case PlanMsg.ToggleGroupSummaryExpanded( group ) =>
      copy( productionUi = productionUi.toggleGroupSummaryExpanded( group ) ) -> Cmd.None
    case PlanMsg.ToggleGroupSummaryFlat( group ) =>
      copy( productionUi = productionUi.toggleGroupSummaryFlat( group ) ) -> Cmd.None
    case PlanMsg.MoveTo( None ) =>
      this -> MoveTo.top
    case PlanMsg.MoveTo( Some( anchor ) ) =>
      this -> MoveTo.element( anchor )
    case PlanMsg.ToggleProductionRowExpanded( recipe ) =>
      copy( productionUi = productionUi.toggleProductionRowExpanded( recipe ) ) -> Cmd.None
    case PlanMsg.MoveProductionRow( rows, index, amount ) =>
      copy( productionUi = productionUi.moveProductionRow( rows )( index, amount ) ) ->
        Cmd.None
    case PlanMsg.ToggleMarkComplete( recipe ) =>
      copy( productionUi = productionUi.toggleMarkComplete( recipe ) ) -> Cmd.None
    case PlanMsg.ToggleShowAllFlows( enable ) =>
      copy( productionUi = productionUi.setShowAllFlows( enable ) ) -> Cmd.None
    case PlanMsg.Flow( action ) =>
      copy( flows = flows.map( _.update( action ) ) ) -> Cmd.None
    case PlanMsg.SetGroup( endId, splitId, group ) =>
      copy( flows = flows.map( _.setGroup( endId, splitId, group ) ) ) -> Cmd.None
    case PlanMsg.SwapGroups( from, to ) =>
      copy( flows = flows.map( _.swapGroups( from, to ) ) ) -> Cmd.None
    case PlanMsg.SendSolverRequest =>
      copy( ui = ui.setComputing( true ) ) ->
        http.computeSolution( solverRequest ).map( PlanMsg.ReceiveSolverResponse( solverRequest, _ ) )
    case PlanMsg.ReceiveSolverResponse( solverRequest, solverResponse ) =>
      copy( ui = ui.setComputing( false ), solution = SolutionModel( solverRequest, solverResponse ).some ) -> Cmd.None
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
    recipeOptions = if ( ui.options.contains( SidePanel.Recipes ) ) recipeOptions.restore else recipeOptions,
    resourceOptions =
      if ( ui.options.contains( SidePanel.ResourceNodes ) ) resourceOptions.restore else resourceOptions,
    powerOptions = if ( ui.options.contains( SidePanel.Power ) ) powerOptions.restore else powerOptions,
    requests = if ( ui.options.contains( SidePanel.Requests ) ) requests.restore else requests
  )

  def setTab( tab: Option[SidePanel], organizer: Boolean ): PlanModel =
    copy( ui = ui.setTab( tab, organizer ) )

  def getLocation: LocationModel.Plan = LocationModel.Plan( ui.options, name.saved._1F, organizer = ui.isOrganizer )

  def locationToOpenOptions: LocationModel.Plan                  = getLocation.copy( options = ui.hasOptions.some )
  def locationToCloseOptions: LocationModel.Plan                 = getLocation.copy( options = none )
  def locationToOpenRequests: LocationModel.Plan                 = getLocation.copy( options = SidePanel.Requests.some )
  def locationToCloseRequests: LocationModel.Plan                = getLocation.copy( options = none )
  def locationToOpenOption( tab: SidePanel ): LocationModel.Plan = getLocation.copy( options = tab.some )
  def locationWithPlanId( planId: Option[PlanId] ): LocationModel.Plan = getLocation.copy( id = planId )

  lazy val solverRequest: SolverRequest =
    SolverRequest(
      env.game.version.version,
      requests.requested,
      recipeOptions.allowedRecipes ++
        env.game.powerRecipes
          .filter( rec => rec.products.nonEmpty && powerOptions.allowedGenerators.contains( rec.producedIn.className ) )
          .map( _.className ),
      extractionOptions.resources( env, resourceOptions.resourceNodes ),
      logisticsOptions.belts( env ).last.className,
      logisticsOptions.pipelines( env ).last.className,
      powerOptions.maxProductionBoost,
      powerOptions.manufacturingClockSpeed
    )

  private def solutionDirty( solution: SolutionModel ): Boolean =
    solution.response.solution.isEmpty || solution.requested != solverRequest

  lazy val canCompute: Boolean =
    if ( requests.requested.isEmpty )
      solution.exists( solutionDirty )
    else
      solution.forall( solutionDirty )

  val save: pp.Plan =
    pp.Plan(
      name.name,
      env.game.version.version,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requests,
      solution.flatMap( sm => sm.response.solution.tupleLeft( sm.requested ) ),
      flows.fold( identity, flows => flows ),
      productionUi
    )

  val dirty: Boolean = name.saved.forall( _._2 != save )

object PlanModel:
  private def productionOf(
      env: Env,
      resourceOptions: ResourceOptionsInputModel,
      extractionOptions: ExtractionOptions,
      logisticsOptions: LogisticsOptions,
      requestSelection: RequestsModel,
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

  private def apply(
      env: Env,
      ui: PlanModel.Ui,
      name: PlanNameModel,
      recipeOptions: RecipeOptionsInputModel,
      resourceOptions: ResourceOptionsInputModel,
      extractionOptions: ExtractionOptions,
      logisticsOptions: LogisticsOptions,
      powerOptions: PowerOptions,
      requests: RequestsModel,
      solution: Option[SolutionModel],
      productionUi: ProdModel.Ui,
      savedFlows: Option[pp.Flows]
  ): PlanModel =
    val production: ProdModel =
      PlanModel.productionOf( env, resourceOptions, extractionOptions, logisticsOptions, requests, solution )
    val flows: Either[pp.Flows, Flows] =
      ( solution *> savedFlows )
        .map( Flows.from( production, _ ).asRight )
        .orElse( savedFlows.map( _.asLeft ) )
        .getOrElse( Flows.init( production ).asRight )
    new PlanModel(
      env,
      ui,
      name,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requests,
      solution,
      production,
      productionUi,
      flows
    )

  def init( env: Env, ui: Ui = Ui.init ): PlanModel =
    PlanModel(
      env,
      ui,
      PlanNameModel.init( PlanName( "New plan" ), saved = None ),
      RecipeOptionsInputModel.init( env ),
      ResourceOptionsInputModel.init( env.game.defaultResourceOptions ),
      ExtractionOptions.init( env.game ),
      LogisticsOptions.init( env ),
      PowerOptions.init( env ),
      RequestsModel.init,
      none,
      ProdModel.Ui.init,
      none
    )

  def load( env: Env, ui: Ui, planId: PlanId, saved: pp.Plan ): ( PlanModel, Cmd[Nothing, PlanMsg] ) =
    val model =
      PlanModel(
        env = env,
        ui = ui,
        name = PlanNameModel.init( saved.name, saved = ( planId, saved ).some ),
        recipeOptions = RecipeOptionsInputModel.from( saved.recipeOptions ),
        resourceOptions = ResourceOptionsInputModel.from( env, saved.resourceOptions ).restore,
        extractionOptions = ExtractionOptions.from( saved.extractionOptions ),
        logisticsOptions = LogisticsOptions.from( env, saved.logisticsOptions ),
        powerOptions = PowerOptions.from( saved.powerOptions ),
        requests = RequestsModel.from( env, saved.requestSelection ).restore,
        solution = saved.solution.map { case ( req, res ) => SolutionModel( req, res ) },
        productionUi = ProdModel.Ui.from( saved.flows.prodHash, saved.productionUi ),
        savedFlows = saved.flows.some
      )
    model -> ( if ( model.solution.isEmpty && model.canCompute ) Cmd.Emit( PlanMsg.SendSolverRequest ) else Cmd.None )

  case class Ui(
      hasOptions: SidePanel & SidePanel.OptionsTab,
      options: Option[SidePanel],
      requestSelectionVisible: Boolean,
      isOrganizer: Boolean,
      isComputing: Boolean
  ):
    def setComputing( computing: Boolean ): Ui = copy( isComputing = computing )

    def setTab( optionsTab: Option[SidePanel], organizer: Boolean ): Ui =
      copy(
        hasOptions = optionsTab.flatMap( _.optionsTab ).getOrElse( this.hasOptions ),
        options = optionsTab,
        isOrganizer = organizer
      )

    def isRequests: Boolean = options.contains( SidePanel.Requests )

  object Ui:
    val init: Ui = Ui(
      hasOptions = SidePanel.Recipes,
      options = none,
      requestSelectionVisible = false,
      isOrganizer = false,
      isComputing = false
    )
