package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import protocol.persistence.PlanId
import protocol.persistence.PlanName
import spa.plan.PlanNameModel

object LocalPlanNameModel:
  case class Saved( name: PlanName, saved: Option[( PlanId, pp.Plan )] ) derives ConfiguredEncoder
  object Saved:
    def apply( model: PlanNameModel ): Saved =
      Saved( model.name, model.saved )

  case class Loaded( name: PlanName, saved: Option[( PlanId, pp.Plan )] ) derives ConfiguredDecoder:
    def toPlanNameModel: PlanNameModel = PlanNameModel.init( name, saved )
