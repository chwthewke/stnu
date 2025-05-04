package net.chwthewke.stnu
package model

import cats.Order
import cats.Show
import cats.derived.strict.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

enum RecipeCategory( val tierOpt: Option[Tier] ) derives Show, Order, ConfiguredDecoder, ConfiguredEncoder:
  case Extraction( tier: Option[Tier] ) extends RecipeCategory( tier )
  case PowerGeneration( tier: Tier )    extends RecipeCategory( Some( tier ) )
  case Milestone( tier: Tier )          extends RecipeCategory( Some( tier ) ) with RecipeCategory.Manufacturing
  case Alternate( tier: Tier )          extends RecipeCategory( Some( tier ) ) with RecipeCategory.Manufacturing
  case Mam( tier: Tier, research: ResearchCategory )
      extends RecipeCategory( Some( tier ) )
      with RecipeCategory.Manufacturing

object RecipeCategory:
  sealed trait Manufacturing:
    def tier: Tier
