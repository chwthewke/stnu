package net.chwthewke.stnu
package game

import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto.deriveReader

import model.ExtractorType
import model.Footprint
import model.Item
import model.Machine
import model.ResourceDistrib

case class ExtraGameDataConfig(
    resourceNodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
    buildingFootprints: Map[ClassName[Machine], Footprint],
    maxProductionBoostShards: Int
) derives Show

object ExtraGameDataConfig:
  private given ConfigReader[Footprint] = deriveReader[Footprint]

  private given ConfigReader[ResourceDistrib] =
    ConfigReader[Vector[Int]]
      .emap( counts =>
        ( counts.lift( 0 ), counts.lift( 1 ), counts.lift( 2 ) )
          .mapN( ResourceDistrib( _, _, _ ) )
          .toRight(
            CannotConvert( counts.mkString( "[", ", ", "]" ), "ResourceDistrib", "the array must have 3 elements" )
          )
      )

  private given crmea[A]( using reader: ConfigReader[Map[String, A]] ): ConfigReader[Map[ExtractorType, A]] =
    reader.emap(
      _.toVector
        .traverse:
          case ( k, v ) =>
            ExtractorType
              .withNameOption( k )
              .toRight( CannotConvert( k, "ExtractorType", "Unknown key" ) )
              .tupleRight( v )
        .map( _.toMap )
    )

  private given crmca[C, A: ConfigReader]: ConfigReader[Map[ClassName[C], A]] =
    ConfigReader[Map[String, A]].map( _.map:
      case ( k, v ) => ( ClassName( k ), v ) )

  given ConfigReader[ExtraGameDataConfig] = deriveReader[ExtraGameDataConfig]
