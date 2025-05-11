package net.chwthewke.stnu
package game

import cats.Show
import cats.syntax.all.*

enum AnalysisItem:
  override def toString: String = show"${this.displayName} [${this.className}]"
  case OfItem( item: GameItem )            extends AnalysisItem
  case OfRecipe( recipe: GameRecipe )      extends AnalysisItem
  case OfSchematic( schematic: Schematic ) extends AnalysisItem

object AnalysisItem:
  extension ( analysisItem: AnalysisItem )
    def className: ClassName[Any] = analysisItem match
      case OfItem( item )           => item.className
      case OfRecipe( recipe )       => recipe.className
      case OfSchematic( schematic ) => schematic.className
    def displayName: String = analysisItem match
      case OfItem( item )           => item.displayName
      case OfRecipe( recipe )       => recipe.displayName
      case OfSchematic( schematic ) => schematic.displayName

  given Show[AnalysisItem] = Show.fromToString
