package net.chwthewke.stnu
package spa
package plan

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine
import model.Recipe
import model.ResourcePurity
import model.Tier
import model.Transport
import model.prod.Group
import protocol.persistence.PlanId
import protocol.persistence.ProcessSplitId
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse
import spa.prod.EndId
import spa.prod.FlowAction
import spa.prod.ProdModel

enum PlanMsg:
  case PlanName( action: PlanNameAction )
  case SetResourceDistribution(
      extractorType: ExtractorType,
      item: ClassName[Item],
      purity: ResourcePurity,
      value: String
  )
  case SetExtractionOption( extractionOption: ExtractionOption )
  case SetLogisticsOption( logisticsOption: LogisticsOption )
  case SetRecipeOption( recipeOption: RecipeOption )
  case SetPowerOption( powerOption: PowerOption )
  case Requests( action: RequestsAction )
  case ToggleRequestSelection( enable: Boolean )
  // TODO extract the "ProdModel.Ui"/"Flows" messages?
  case ToggleProductionRowExpanded( row: ProcessSplitId )
  case ToggleProductionSummaryExpanded( open: Boolean )
  case ToggleMarkComplete( row: ProcessSplitId )
  case MoveProductionRow( rows: Vector[ProdModel.Row], index: ProcessSplitId, amount: Int )
  case ToggleShowAllFlows( showAll: Boolean )
  case Flow( action: FlowAction )
  case SetGroup( endId: EndId, splitId: ProcessSplitId, group: Group )
  case SwapGroups( from: Group, to: Group )
  case ToggleGroupSummaryExpanded( group: Group )
  case ToggleGroupSummaryFlat( group: Group )
  case MoveTo( anchor: Option[String] )
  //
  case SendSolverRequest
  case ReceiveSolverResponse( request: SolverRequest, solution: SolverResponse )
  case SaveRequest( confirm: Boolean )
  case PlanLoaded( id: PlanId, plan: Option[pp.Plan] )
  case RevertPlan
  case ClearPlan

enum ExtractionOption:
  case SetMiner( machine: ClassName[Machine] )
  case SetClockSpeed( clockSpeed: ClockSpeedPreset.Extraction )
  case ToggleExcludeWaterPumpFromOverclocking( enable: Boolean )
  case ToggleExtractorType( extractor: ExtractorType, value: String )
  case ToggleFrackingPreference( item: ClassName[Item], value: String )
  case SetResourceWeight( item: ClassName[Item], value: String )

enum LogisticsOption:
  case SetBelt( belt: ClassName[Transport] )
  case SetPipeline( pipeline: ClassName[Transport] )
  case ToggleUseAll( enable: Boolean )
  case SelectBelt( belt: ClassName[Transport], enable: Boolean )
  case SelectPipeline( pipeline: ClassName[Transport], enable: Boolean )

enum RecipeOption:
  case Reset
  case SetCurrent
  case SetRecipe( recipe: ClassName[Recipe.Manufacturing], enable: Boolean )
  case SetMaxTier( tier: Tier, withAlts: Boolean )
  case ToggleAlts( enable: Boolean )
  case ToggleConversion( enable: Boolean )
  case SearchInput( value: String )
  case SearchReset
  case ToggleHideFicsmas( enable: Boolean )

enum PowerOption:
  case Reset
  case SetPowerGenerator( generator: ClassName[Machine], enable: Boolean )
  case SetMaxProductionBoost( value: String )
  case SetManufacturingClockSpeed( clockSpeedPreset: ClockSpeedPreset )

enum RequestsAction:
  case Delete( item: ClassName[Item] )
  case SetAmountValue( item: ClassName[Item], value: String )
  case SearchInput( value: String )
  case SearchReset
  case RequestItem( item: Item )

enum PlanNameAction:
  case EditStart
  case EditCancel
  case EditCommit
  case EditSetValue( value: String )
  case SaveResponse( id: Option[PlanId], saved: pp.Plan )
  case SaveCancel
  case Duplicate
  case Revert
  case RevertCancel
  case Clear
  case ClearCancel
