package net.chwthewke.stnu
package model

import cats.Applicative
import cats.Order
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder

enum RecipeCategory derives Show, Order, ConfiguredDecoder, ConfiguredEncoder:
  case Extraction( tier: Tier )                      extends RecipeCategory
  case PowerGeneration( tier: Tier )                 extends RecipeCategory with RecipeCategory.NonExtraction_
  case Milestone( tier: Tier )                       extends RecipeCategory with RecipeCategory.Manufacturing_
  case Alternate( tier: Tier )                       extends RecipeCategory with RecipeCategory.Manufacturing_
  case Mam( tier: Tier, research: ResearchCategory ) extends RecipeCategory with RecipeCategory.Manufacturing_

object RecipeCategory:
  extension ( category: RecipeCategory )
    def tier: Tier = category match
      case RecipeCategory.Extraction( tier )      => tier
      case RecipeCategory.PowerGeneration( tier ) => tier
      case RecipeCategory.Milestone( tier )       => tier
      case RecipeCategory.Alternate( tier )       => tier
      case RecipeCategory.Mam( tier, _ )          => tier

    def modifyFTier[F[_]: Applicative]( f: Tier => F[Tier] ): F[RecipeCategory] =
      category match
        case RecipeCategory.Extraction( tier )      => f( tier ).map( RecipeCategory.Extraction( _ ) )
        case RecipeCategory.PowerGeneration( tier ) => f( tier ).map( RecipeCategory.PowerGeneration( _ ) )
        case RecipeCategory.Milestone( tier )       => f( tier ).map( RecipeCategory.Milestone( _ ) )
        case RecipeCategory.Alternate( tier )       => f( tier ).map( RecipeCategory.Alternate( _ ) )
        case RecipeCategory.Mam( tier, research )   => f( tier ).map( RecipeCategory.Mam( _, research ) )

    def extraction: Option[RecipeCategory.Extraction] =
      category match
        case c: RecipeCategory.Extraction => c.some
        case _                            => none
    def powerGeneration: Option[RecipeCategory.PowerGeneration] =
      category match
        case c: RecipeCategory.PowerGeneration => c.some
        case _                                 => none
    def manufacturing: Option[RecipeCategory.Manufacturing] =
      category match
        case c: RecipeCategory.Milestone => c.some
        case c: RecipeCategory.Alternate => c.some
        case c: RecipeCategory.Mam       => c.some
        case _                           => none

  type Manufacturing = RecipeCategory & RecipeCategory.Manufacturing_
  type NonExtraction = RecipeCategory & RecipeCategory.NonExtraction_

  sealed trait NonExtraction_ :
    def tier: Tier

  sealed trait Manufacturing_ extends NonExtraction_
