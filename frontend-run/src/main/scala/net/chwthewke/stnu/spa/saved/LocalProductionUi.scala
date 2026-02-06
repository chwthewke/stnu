package net.chwthewke.stnu
package spa.saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.Recipe
import spa.prod.ProdModel

object LocalProductionUi:
  case class Saved(
      productionSummaryExpanded: Boolean,
      productionRowExpanded: Option[ClassName[Recipe]],
      productionRowOrder: Option[Vector[Int]],
      validFor: Vector[ClassName[Recipe]]
  ) derives ConfiguredEncoder

  object Saved:
    def apply( ui: ProdModel.Ui ): Saved =
      Saved( ui.productionSummaryExpanded, ui.productionRowExpanded, ui.productionRowOrder, ui.validFor )

  case class Loaded(
      productionSummaryExpanded: Boolean,
      productionRowExpanded: Option[ClassName[Recipe]],
      productionRowOrder: Option[Vector[Int]],
      validFor: Vector[ClassName[Recipe]]
  ) derives ConfiguredDecoder:
    def toProductionUi: ProdModel.Ui =
      ProdModel.Ui( productionSummaryExpanded, productionRowExpanded, productionRowOrder, validFor )
