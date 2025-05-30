package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import model.Recipe
import spa.plan.PlanModel

object LocalPlanModel:
  case class Saved(
      recipeOptions: LocalRecipeOptions.Saved,
      resourceOptions: LocalResourceOptions.Saved,
      extractionOptions: LocalExtractionOptions.Saved,
      logisticsOptions: LocalLogisticsOptions.Saved,
      powerOptions: LocalPowerOptions.Saved,
      requestSelection: LocalRequestSelection.Saved,
      solutionComputed: Boolean,
      expandedProductionRow: Option[ClassName[Recipe]],
      expandedProductionSummary: Boolean
  ) derives ConfiguredEncoder

  object Saved:
    def apply( planModel: PlanModel ): Saved =
      Saved(
        LocalRecipeOptions.Saved( planModel.recipeOptions ),
        LocalResourceOptions.Saved( planModel.resourceOptions ),
        LocalExtractionOptions.Saved( planModel.extractionOptions ),
        LocalLogisticsOptions.Saved( planModel.env, planModel.logisticsOptions ),
        LocalPowerOptions.Saved( planModel.powerOptions ),
        LocalRequestSelection.Saved( planModel.requestSelection ),
        !planModel.canCompute,
        planModel.ui.productionRowExpanded,
        planModel.ui.productionSummaryExpanded
      )

  case class Loaded(
      recipeOptions: LocalRecipeOptions.Loaded,
      resourceOptions: LocalResourceOptions.Loaded,
      extractionOptions: LocalExtractionOptions.Loaded,
      logisticsOptions: LocalLogisticsOptions.Loaded,
      powerOptions: LocalPowerOptions.Loaded,
      requestSelection: LocalRequestSelection.Loaded,
      solutionComputed: Boolean,
      expandedProductionRow: Option[ClassName[Recipe]],
      expandedProductionSummary: Boolean
  ) derives ConfiguredDecoder:
    def patch( planModel: PlanModel ): ( PlanModel, Boolean ) =
      (
        planModel.copy(
          recipeOptions = recipeOptions.toRecipeOptions,
          resourceOptions = resourceOptions.toResourceOptions,
          extractionOptions = extractionOptions.toExtractionOptions,
          logisticsOptions = logisticsOptions.toLogisticsOptions,
          powerOptions = powerOptions.toPowerOptions,
          requestSelection = requestSelection.toRequestSelection,
          ui = planModel.ui.copy(
            productionRowExpanded = expandedProductionRow,
            productionSummaryExpanded = expandedProductionSummary
          )
        ),
        solutionComputed
      )
