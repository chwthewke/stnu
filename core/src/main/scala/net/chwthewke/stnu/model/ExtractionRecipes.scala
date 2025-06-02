package net.chwthewke.stnu
package model

import cats.Traverse
import cats.data.ValidatedNel
import cats.derived.strict.*
import cats.syntax.all.*
import scala.collection.immutable.SortedMap

enum ExtractionRecipes:
  case Fixed( recipe: Recipe.Extraction )
  case Variable( byPurity: ExtractionRecipes.ByPurity[Recipe.Extraction] )

object ExtractionRecipes:
  extension ( extractionRecipes: ExtractionRecipes )
    def recipes: Vector[Recipe.Extraction] =
      extractionRecipes match
        case ExtractionRecipes.Fixed( recipe )      => Vector( recipe )
        case ExtractionRecipes.Variable( byPurity ) => ResourcePurity.cases.map( byPurity.get )

  sealed trait ByPurity[R] derives Traverse:
    def get( purity: ResourcePurity ): R

  object ByPurity:

    case class Impl[R]( map: SortedMap[ResourcePurity, R] ) extends ByPurity[R]:
      override def get( purity: ResourcePurity ): R = map( purity )

    def apply( vector: Vector[( ResourcePurity, Recipe.Extraction )] ): ValidatedNel[String, ExtractionRecipes] =
      val map: SortedMap[ResourcePurity, Recipe.Extraction] = vector.to( SortedMap )
      ResourcePurity.cases
        .traverseVoid: purity =>
          if ( map.contains( purity ) ) ().validNel
          else purity.invalidNel
        .as( ExtractionRecipes.Variable( new Impl( map ) ) )
        .leftMap( missing => show"Missing resource purities ${missing.mkString_( ", " )}" )
        .toValidatedNel
