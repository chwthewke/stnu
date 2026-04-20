package net.chwthewke.stnu
package spa.prod

import cats.syntax.all.*
import scala.collection.immutable.SortedMap

case class ShownFlows( self: Flows ):

  private def showProdRecipes: String =
    self.prod.productionRows
      .map: cr =>
        f"${cr.fractionalAmount}%.3f ${cr.recipe.className}%s"
      .mkString_( "\n  " )

  override def toString: String =
    s"""FLOWS
       |Prod Recipes
       |  $showProdRecipes
       |Splits
       |${self.endSplits.to( SortedMap ).map( Shown.showSplits ).mkString( "\n" )}
       |By Split id
       |${self.endsBySplitId.to( SortedMap ).map( Shown.showEndSplit ).mkString( "\n" )}
       |Transports (ref)
       |${self.itemFlows.to( SortedMap ).map( Shown.showItemFlowRef ).mkString( "\n" )}
       |Transports
       |${self.itemTransports.to( SortedMap ).map( Shown.showItemFlow ).mkString( "\n" )}
       |""".stripMargin
