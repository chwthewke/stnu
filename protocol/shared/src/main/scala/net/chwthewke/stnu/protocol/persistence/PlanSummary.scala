package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec
import java.time.Instant

import model.Item

case class PlanSummary(
    planId: PlanId,
    name: PlanName,
    updated: Instant,
    requested: Vector[( ClassName[Item], Double )]
) derives ConfiguredCodec
