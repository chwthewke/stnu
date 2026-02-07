package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import spa.prod.Flows

object LocalFlows:
  case class Saved( flows: pp.Flows ) derives ConfiguredEncoder

  object Saved:
    def apply( flows: Either[pp.Flows, Flows] ): Saved = Saved( flows.map( f => f: pp.Flows ).merge )

  case class Loaded( flows: pp.Flows ) derives ConfiguredDecoder
