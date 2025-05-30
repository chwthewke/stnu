package net.chwthewke.stnu
package spa
package plan

import model.ExtractorType
import model.Item
import model.Machine
import model.Recipe
import model.ResourcePurity
import model.Tier
import model.Transport
import spa.prod.ClockSpeedPreset

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
