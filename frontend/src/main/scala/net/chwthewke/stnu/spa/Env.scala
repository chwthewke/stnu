package net.chwthewke.stnu
package spa

import org.http4s.Uri

import model.Item
import model.Machine
import model.Model

// NOTE general purpose "environment" (things that change only when switching game versions),
//  containing generally required content-related information
case class Env(
    game: Model,
    itemIcons: Map[ClassName[Item], Uri],
    machineIcons: Map[ClassName[Machine], Uri]
)
