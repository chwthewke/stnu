package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

case class Plan(
    name: PlanName,
    recipeOptions: RecipeOptions,
    resourceOptions: ResourceOptions,
    extractionOptions: ExtractionOptions,
    logisticsOptions: LogisticsOptions,
    powerOptions: PowerOptions,
    requestSelection: RequestSelection
) derives ConfiguredCodec
