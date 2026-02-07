package net.chwthewke.stnu
package spa.saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.prod.Group
import protocol.persistence.ProcessSplitId
import spa.prod.ProdModel

object LocalProductionUi:
  case class Saved(
      prodHash: Option[ProdModel.Hash],
      productionSummaryExpanded: Boolean,
      expandedGroupSummaries: Vector[Group],
      detailedGroupSummaries: Vector[Group],
      expandedRows: Vector[ProcessSplitId],
      rowOrder: Option[Vector[ProcessSplitId]],
      completed: Vector[ProcessSplitId],
      showAllFlows: Boolean
  ) derives ConfiguredEncoder

  object Saved:
    def apply( ui: ProdModel.Ui ): Saved =
      Saved(
        ui.prodHash,
        ui.productionSummaryExpanded,
        ui.expandedGroupSummaries.toVector,
        ui.detailedGroupSummaries.toVector,
        ui.expandedRows.toVector,
        ui.rowOrder,
        ui.completed.toVector,
        ui.showAllFlows
      )

  case class Loaded(
      prodHash: Option[ProdModel.Hash],
      productionSummaryExpanded: Boolean,
      expandedGroupSummaries: Vector[Group],
      detailedGroupSummaries: Vector[Group],
      expandedRows: Vector[ProcessSplitId],
      rowOrder: Option[Vector[ProcessSplitId]],
      completed: Vector[ProcessSplitId],
      showAllFlows: Boolean
  ) derives ConfiguredDecoder:
    def toProductionUi: ProdModel.Ui =
      ProdModel.Ui(
        prodHash,
        productionSummaryExpanded,
        expandedGroupSummaries.toSet,
        detailedGroupSummaries.toSet,
        expandedRows.toSet,
        rowOrder,
        completed.toSet,
        showAllFlows
      )
