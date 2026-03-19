package net.chwthewke.stnu
package protocol
package persistence

import io.circe.derivation.ConfiguredCodec

import model.Recipe

case class RecipeOptions(
    hideFicsmas: Boolean,
    allowedRecipes: Set[ClassName[Recipe.Manufacturing]]
) derives ConfiguredCodec
