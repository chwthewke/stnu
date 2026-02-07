package net.chwthewke.stnu
package spa
package plan

import cats.syntax.all.*

enum SidePanel:
  case Recipes       extends SidePanel with SidePanel.OptionsTab
  case ResourceNodes extends SidePanel with SidePanel.OptionsTab
  case ResourcePrefs extends SidePanel with SidePanel.OptionsTab
  case Logistics     extends SidePanel with SidePanel.OptionsTab
  case Power         extends SidePanel with SidePanel.OptionsTab
  case Requests

  def hasOptions: Boolean = this match
    case _: SidePanel.OptionsTab => true
    case _                       => false

  def optionsTab: Option[SidePanel & SidePanel.OptionsTab] =
    this match
      case ot: SidePanel.OptionsTab => ot.some
      case _                        => none

object SidePanel extends CustomEnum[SidePanel]:
  sealed trait OptionsTab

  override def keyOf( tab: SidePanel ): String =
    tab.toString.toLowerCase

  extension ( tab: SidePanel )
    def description: String =
      tab match
        case SidePanel.ResourceNodes => "Res. nodes"
        case SidePanel.ResourcePrefs => "Res. prefs"
        case SidePanel.Logistics     => "Logistics"
        case SidePanel.Recipes       => "Recipes"
        case SidePanel.Power         => "Power"
        case SidePanel.Requests      => "Requests"
