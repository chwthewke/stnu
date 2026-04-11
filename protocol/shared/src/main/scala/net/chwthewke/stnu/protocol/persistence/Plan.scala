package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import solver.SolverRequest
import solver.SolverResponse

case class Plan(
    name: PlanName,
    modelVersionId: ModelVersionId,
    recipeOptions: RecipeOptions,
    resourceOptions: ResourceOptions,
    extractionOptions: ExtractionOptions,
    logisticsOptions: LogisticsOptions,
    powerOptions: PowerOptions,
    requestSelection: RequestSelection,
    solution: Option[( SolverRequest, SolverResponse.Solution )],
    flows: Flows,
    productionUi: ProductionUi
) derives ConfiguredCodec
