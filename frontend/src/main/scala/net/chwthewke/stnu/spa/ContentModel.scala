package net.chwthewke.stnu
package spa

import cats.syntax.all.*
import org.http4s.Uri

import model.Item
import model.Machine
import model.ModelIndex
import protocol.game.FullModel

case class ContentModel(
    modelIndex: ModelIndex,
    model: FullModel,
    links: Links
):
  lazy val env: Env =
    val itemIcons: Map[ClassName[Item], Uri] =
      model.game.items
        .mapFilter: item =>
          model.icons.getIconPath( item.className ).map( links.image )

    val machineIcons: Map[ClassName[Machine], Uri] =
      model.game.machines
        .mapFilter: machine =>
          model.icons.getIconPath( machine.className ).map( links.image )

    Env( model.game, itemIcons, machineIcons )
