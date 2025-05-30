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
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

enum PlanMsg:
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
  case RequestSelection( action: RequestSelectionAction )
  case ToggleRequestSelection( enable: Boolean )
  case SendSolverRequest
  case ReceiveSolverResponse( request: SolverRequest, solution: SolverResponse )
  case ToggleProductionRowExpanded( recipe: ClassName[Recipe] )
  case ToggleProductionSummaryExpanded( open: Boolean )

enum ExtractionOption:
  case SetMiner( machine: ClassName[Machine] )
  case SetClockSpeed( clockSpeed: ClockSpeedPreset )
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

enum RequestSelectionAction:
  case SearchInput( value: String )
  case SearchReset
  case RequestItem( item: ClassName[Item] )
  case EditAmountStart( item: ClassName[Item] )
  case EditAmountCancel
  case EditAmountCommit
  case EditAmountDelete
  case EditAmountSetValue( value: String )
