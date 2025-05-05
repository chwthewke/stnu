package net.chwthewke.stnu
package spa

import model.IconIndex
import model.Model
import model.ModelIndex

case class GameModel(
    modelIndex: ModelIndex,
    current: Model,
    icons: IconIndex
)
