package net.chwthewke.stnu
package spa.browse

enum BrowseMsg:
  case SearchItems( search: String )
  case ResetItemSearch
  case SearchRecipes( search: String )
  case ResetRecipeSearch
  case SelectItem( name: String )
  case Sort( by: BrowseModel.Sort )
  case ToggleHideFicsmas( enable: Boolean )
