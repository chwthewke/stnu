package net.chwthewke.stnu
package spa
package plan

enum OptionsTab:
  case ResourceNodes
  case ResourcePrefs
  case Logistics
  case Recipes

object OptionsTab:
  extension ( tab: OptionsTab )
    def description: String =
      tab match
        case OptionsTab.ResourceNodes => "Res. nodes"
        case OptionsTab.ResourcePrefs => "Res. prefs"
        case OptionsTab.Logistics     => "Logistics"
        case OptionsTab.Recipes       => "Recipes"
