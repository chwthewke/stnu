package net.chwthewke.stnu
package protocol
package solver

import cats.syntax.all.*
import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.derivation.ConfiguredCodec
import org.scalacheck.Gen
import scala.collection.immutable.SortedMap

import model.Tier

private given KeyDecoder[Tier] = KeyDecoder[Int].map( Tier( _ ) )
private given KeyEncoder[Tier] = KeyEncoder[Int].contramap( _.value )

case class ExportedSolutions(
    solutionsByTierAndSize: SortedMap[Tier, SortedMap[Int, Vector[( SolverRequest, SolverResponse )]]]
) derives ConfiguredCodec:
  def sample( tier: Tier, size: Int ): Gen[( SolverRequest, SolverResponse )] =
    solutionsByTierAndSize
      .minAfter( tier )
      ._2F
      .flatMap( _.maxBefore( size + 1 )._2F )
      .flatMap( _.toNev )
      .fold( Gen.fail )( ss => Gen.oneOf( ss.toVector ) )

object ExportedSolutions:
  private given KeyDecoder[ModelVersionId] = KeyDecoder[Int].map( ModelVersionId( _ ) )
  private given KeyEncoder[ModelVersionId] = KeyEncoder[Int].contramap( _.id )

  case class ByModelVersion( seed: Long, byModelVersion: Map[ModelVersionId, ExportedSolutions] )
      derives ConfiguredCodec
