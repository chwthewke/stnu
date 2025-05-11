package net.chwthewke.stnu
package game

import model.Tier

type Milestone = Tier

object Milestone:
  val Zero: Milestone                          = Tier( 0 )
  def apply( schematic: Schematic ): Milestone = Tier( schematic.techTier )
