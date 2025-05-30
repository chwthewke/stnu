package net.chwthewke.stnu
package spa
package plan

enum OptionsTab:
  case Recipes
  case ResourceNodes
  case ResourcePrefs
  case Logistics

object OptionsTab extends CustomEnum[OptionsTab]:
  override def keyOf( tab: OptionsTab ): String =
    tab.toString.toLowerCase

  extension ( tab: OptionsTab )
    def description: String =
      tab match
        case OptionsTab.ResourceNodes => "Res. nodes"
        case OptionsTab.ResourcePrefs => "Res. prefs"
        case OptionsTab.Logistics     => "Logistics"
        case OptionsTab.Recipes       => "Recipes"
