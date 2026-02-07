package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.OptionT
import cats.syntax.all.*
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.ResourceDistrib
import model.Tier
import model.Transport
import protocol.solver.ExportedSolutions
import spa.plan.ExtractionOptions
import spa.plan.RequestsModel

object ProdModelGenerators:

  def prodModel( env: Env, solutions: ExportedSolutions )(
      maxTierGen: Gen[Tier] = Gen.choose( 2, 9 ).map( Tier( _ ) ),
      requestSizeGen: Gen[Int] = Gen.choose( 1, 20 ),
      hasSolutionGen: Gen[Boolean] = true
  ): Gen[ProdModel] =
    (
      requestSelection( env )( requestSizeGen ),
      OptionT.whenM( hasSolutionGen )( solution( env, solutions )( maxTierGen, requestSizeGen ) ).value,
      resourceNodes( env ),
      extractionOptions( env ),
      pickTransports( env.game.conveyorBelts ),
      pickTransports( env.game.pipelines )
    ).mapN( ( request, solution, nodes, extraction, belts, pipelines ) =>
      ProdModel( env, request, solution, nodes, extraction, belts, pipelines )
    )

  def pickTransports( from: NonEmptyVector[Transport] ): Gen[NonEmptyList[Transport]] =
    Gen
      .choose( 1, from.length )
      .flatMap( n => Gen.pick( n, from.toVector ).map( s => NonEmptyList.fromListUnsafe( s.toList ) ) )

  def requestSelection( env: Env )( requestSizeGen: Gen[Int] = Gen.choose( 1, 20 ) ): Gen[RequestsModel] =
    requestSizeGen
      .flatMap( Gen.pick( _, env.game.items.values.toSeq ) )
      .flatMap: items =>
        items.toList.traverse: item =>
          Gen.choose( 1, 10 ).map( amt => ( item, InputModel.withDefault( amt.toDouble.toString ) ) )
      .map( RequestsModel( _, SearchQuery.init ) )

  def solution( env: Env, solutions: ExportedSolutions )(
      tierGen: Gen[Tier] = Gen.choose( 2, 9 ).map( Tier( _ ) ),
      sizeGen: Gen[Int] = Gen.choose( 1, 20 )
  ): Gen[ProdModel.Solution] =
    ( tierGen.map( _.min( Tier( 9 ) ) ), sizeGen.map( _.max( 1 ) ) )
      .flatMapN( ( tier, size ) => solutions.sample( tier, size ) )
      .map:
        case ( req, res ) => ProdModel.Solution( env, req, res )

  def resourceNodes( env: Env ): Gen[Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]]] =
    env.game.defaultResourceOptions.resourceNodes

  def extractionOptions( env: Env ): Gen[ExtractionOptions] =
    (
      Gen
        .oneOf( env.game.machines.values.filter( _.machineType.extractor.contains( ExtractorType.Miner ) ) )
        .map( _.className ),
      Gen.oneOf( ClockSpeedPreset.cases )
    ).mapN(
      ExtractionOptions( _, _, ExtractorType.cases.toSet, Set.empty, Map.empty )
    )
