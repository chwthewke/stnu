package net.chwthewke.stnu
package spa
package prod

def showSrcDest( sd: SrcDest ): String = sd match
  case SrcDest.Extract( recipe ) => recipe.recipe.displayName
  case SrcDest.Step( recipe )    => recipe.recipe.displayName
  case SrcDest.Input             => "INPUT"
  case SrcDest.Requested         => "REQUEST"
  case SrcDest.Byproduct         => "BYPRODUCT"
