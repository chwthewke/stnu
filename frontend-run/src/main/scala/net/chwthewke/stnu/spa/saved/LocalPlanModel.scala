package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import spa.plan.PlanModel

object LocalPlanModel:
  case class Saved(
      recipeOptions: LocalRecipeOptions.Saved,
      resourceOptions: LocalResourceOptions.Saved,
      extractionOptions: LocalExtractionOptions.Saved,
      logisticsOptions: LocalLogisticsOptions.Saved
  ) derives ConfiguredEncoder

  object Saved:
    def apply( planModel: PlanModel ): Saved =
      Saved(
        LocalRecipeOptions.Saved( planModel.recipeOptions ),
        LocalResourceOptions.Saved( planModel.resourceOptions ),
        LocalExtractionOptions.Saved( planModel.extractionOptions ),
        LocalLogisticsOptions.Saved( planModel.env, planModel.logisticsOptions )
      )

  case class Loaded(
      recipeOptions: LocalRecipeOptions.Loaded,
      resourceOptions: LocalResourceOptions.Loaded,
      extractionOptions: LocalExtractionOptions.Loaded,
      logisticsOptions: LocalLogisticsOptions.Loaded
  ) derives ConfiguredDecoder:
    def patch( planModel: PlanModel ): PlanModel =
      planModel.copy(
        recipeOptions = recipeOptions.toRecipeOptions,
        resourceOptions = resourceOptions.toResourceOptions,
        extractionOptions = extractionOptions.toExtractionOptions,
        logisticsOptions = logisticsOptions.toLogisticsOptions
      )
