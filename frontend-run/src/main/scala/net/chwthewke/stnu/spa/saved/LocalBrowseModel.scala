package net.chwthewke.stnu
package spa
package saved

import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

import spa.browse.BrowseModel

object LocalBrowseModel:
  case class Saved(
      itemSearch: Option[String],
      recipeSearch: Option[String],
      topoSort: Boolean,
      hideFicsmas: Boolean
  ) derives ConfiguredEncoder

  object Saved:
    def apply( browseModel: BrowseModel ): Saved =
      Saved(
        fromSearchQuery( browseModel.itemSearch ),
        fromSearchQuery( browseModel.recipeSearch ),
        browseModel.sort == BrowseModel.Sort.Topo,
        browseModel.hideFicsmas
      )

  case class Loaded( itemSearch: Option[String], recipeSearch: Option[String], topoSort: Boolean, hideFicsmas: Boolean )
      derives ConfiguredDecoder:
    def toBrowseModel: BrowseModel =
      BrowseModel(
        toSearchQuery( itemSearch ),
        toSearchQuery( recipeSearch ),
        if ( topoSort ) BrowseModel.Sort.Topo else BrowseModel.Sort.Name,
        hideFicsmas = hideFicsmas
      )
