package net.chwthewke.stnu
package model

import cats.data.ValidatedNel
import cats.syntax.all.*

enum ExtractionRecipes:
  case Fixed( recipe: Recipe.Prod )
  case Variable( byPurity: ExtractionRecipes.ByPurity )

object ExtractionRecipes:
  extension ( extractionRecipes: ExtractionRecipes )
    def recipes: Vector[Recipe.Prod] =
      extractionRecipes match
        case ExtractionRecipes.Fixed( recipe )      => Vector( recipe )
        case ExtractionRecipes.Variable( byPurity ) => ResourcePurity.cases.map( byPurity.get )

  trait ByPurity:
    def get( purity: ResourcePurity ): Recipe.Prod
  object ByPurity:
    private case class Impl( map: Map[ResourcePurity, Recipe.Prod] ) extends ByPurity:
      override def get( purity: ResourcePurity ): Recipe.Prod = map( purity )
    def apply( vector: Vector[( ResourcePurity, Recipe.Prod )] ): ValidatedNel[String, ExtractionRecipes] =
      val map: Map[ResourcePurity, Recipe.Prod] = vector.toMap
      ResourcePurity.cases
        .traverseVoid: purity =>
          if ( map.contains( purity ) ) ().validNel
          else purity.invalidNel
        .as( ExtractionRecipes.Variable( new Impl( map ) ) )
        .leftMap( missing => show"Missing resource purities ${missing.mkString_( ", " )}" )
        .toValidatedNel
