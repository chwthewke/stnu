package net.chwthewke.stnu
package spa

object pp {
  type Plan = protocol.persistence.Plan
  val Plan: protocol.persistence.Plan.type = protocol.persistence.Plan

  type RecipeOptions = protocol.persistence.RecipeOptions
  val RecipeOptions: protocol.persistence.RecipeOptions.type = protocol.persistence.RecipeOptions

  type ResourceOptions = protocol.persistence.ResourceOptions
  val ResourceOptions: protocol.persistence.ResourceOptions.type = protocol.persistence.ResourceOptions

  type ExtractionOptions = protocol.persistence.ExtractionOptions
  val ExtractionOptions: protocol.persistence.ExtractionOptions.type = protocol.persistence.ExtractionOptions

  type LogisticsOptions = protocol.persistence.LogisticsOptions
  val LogisticsOptions: protocol.persistence.LogisticsOptions.type = protocol.persistence.LogisticsOptions

  type PowerOptions = protocol.persistence.PowerOptions
  val PowerOptions: protocol.persistence.PowerOptions.type = protocol.persistence.PowerOptions

  type RequestSelection = protocol.persistence.RequestSelection
  val RequestSelection: protocol.persistence.RequestSelection.type = protocol.persistence.RequestSelection

  type ProductionUi = protocol.persistence.ProductionUi
  val ProductionUi: protocol.persistence.ProductionUi.type = protocol.persistence.ProductionUi

  type EndId = protocol.persistence.EndId
  val EndId: protocol.persistence.EndId.type = protocol.persistence.EndId

  type Flows = protocol.persistence.Flows
  val Flows: protocol.persistence.Flows.type = protocol.persistence.Flows

  type ItemFlows = protocol.persistence.ItemFlows
  val ItemFlows: protocol.persistence.ItemFlows.type = protocol.persistence.ItemFlows
}
