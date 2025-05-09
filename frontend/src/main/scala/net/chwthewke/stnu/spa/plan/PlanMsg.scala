package net.chwthewke.stnu
package spa
package plan

import model.ExtractorType
import model.Item
import model.Machine
import model.ResourcePurity
import spa.prod.ClockSpeed

enum PlanMsg:
  case SetOptionsTab( option: OptionsTab )
  case SetResourceDistribution(
      extractorType: ExtractorType,
      item: ClassName[Item],
      purity: ResourcePurity,
      value: String
  )
  case SetExtractionOption(
      extractionOption: ExtractionOption
  )

enum ExtractionOption:
  case SetMiner( machine: ClassName[Machine] )
  case SetClockSpeed( clockSpeed: ClockSpeed )
  case ToggleExtractorType( extractor: ExtractorType, value: String )
  case ToggleFrackingPreference( item: ClassName[Item], value: String )
  case SetResourceWeight( item: ClassName[Item], value: String )
