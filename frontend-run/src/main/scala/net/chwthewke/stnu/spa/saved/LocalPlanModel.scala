package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import spa.plan.PlanModel

object LocalPlanModel:
  case class Saved(
      name: LocalPlanNameModel.Saved,
      recipeOptions: LocalRecipeOptions.Saved,
      resourceOptions: LocalResourceOptions.Saved,
      extractionOptions: LocalExtractionOptions.Saved,
      logisticsOptions: LocalLogisticsOptions.Saved,
      powerOptions: LocalPowerOptions.Saved,
      requestSelection: LocalRequestSelection.Saved,
      solutionComputed: Boolean,
      prodUi: LocalProductionUi.Saved,
      flows: LocalFlows.Saved
  ) derives ConfiguredEncoder

  object Saved:
    def apply( planModel: PlanModel ): Saved =
      Saved(
        LocalPlanNameModel.Saved( planModel.name ),
        LocalRecipeOptions.Saved( planModel.recipeOptions ),
        LocalResourceOptions.Saved( planModel.resourceOptions ),
        LocalExtractionOptions.Saved( planModel.extractionOptions ),
        LocalLogisticsOptions.Saved( planModel.env, planModel.logisticsOptions ),
        LocalPowerOptions.Saved( planModel.powerOptions ),
        LocalRequestSelection.Saved( planModel.requests ),
        !planModel.canCompute,
        LocalProductionUi.Saved( planModel.productionUi ),
        LocalFlows.Saved( planModel.flows )
      )

  case class Loaded(
      name: LocalPlanNameModel.Loaded,
      recipeOptions: LocalRecipeOptions.Loaded,
      resourceOptions: LocalResourceOptions.Loaded,
      extractionOptions: LocalExtractionOptions.Loaded,
      logisticsOptions: LocalLogisticsOptions.Loaded,
      powerOptions: LocalPowerOptions.Loaded,
      requestSelection: LocalRequestSelection.Loaded,
      solutionComputed: Boolean,
      prodUi: LocalProductionUi.Loaded,
      flows: LocalFlows.Loaded
  ) derives ConfiguredDecoder:
    def patch( planModel: PlanModel ): ( PlanModel, Boolean ) =
      (
        planModel.copy(
          name = name.toPlanNameModel,
          recipeOptions = recipeOptions.toRecipeOptions,
          resourceOptions = resourceOptions.toResourceOptions,
          extractionOptions = extractionOptions.toExtractionOptions,
          logisticsOptions = logisticsOptions.toLogisticsOptions,
          powerOptions = powerOptions.toPowerOptions,
          requests = requestSelection.toRequests,
          productionUi = prodUi.toProductionUi,
          flows = Left( flows.flows )
        ),
        solutionComputed
      )
