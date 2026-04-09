package net.chwthewke.stnu
package protocol
package solver

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Random
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text.utf8
import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*
import org.scalacheck.rng.Seed
import scala.collection.immutable.SortedMap

import model.ExtractorType
import model.Model
import model.ModelConsistency
import model.Tier

object SolutionSampleExporter extends IOApp:

  def solutionRequestAndResponse( model: Model, tier: Tier, size: Int ): Gen[( SolverRequest, SolverResponse )] =
    SolutionGenerators.solverRequestAndResponse( model )(
      requestGen = SolutionGenerators.solverRequest( model )(
        tierGen = tier,
        requestSelectionGen =
          recipes => retry( SolutionGenerators.requestSelection( model )( recipes, sizeGen = _.min( size ) ), 11 ),
        maxProductionBoostGen = _ => 0
      )
    )

  def exportedSolutionsFor(
      model: Model
  ): Gen[ExportedSolutions] =
    ( 1 to 5 )
      .map( n => Tier( ( n * 2 ).min( 9 ) ) )
      .toVector
      .traverse: tier =>
        Vector( 1, 2, 5, 10, 20 )
          .traverse: size =>
            Gen
              .containerOfN[Vector, ( SolverRequest, SolverResponse )](
                10,
                retry( solutionRequestAndResponse( model, tier, size ) )
              )
              .tupleLeft( size )
          .map( _.to( SortedMap ) )
          .tupleLeft( tier )
      .map( _.to( SortedMap ) )
      .map( ExportedSolutions( _ ) )

  def exportedSolutions( models: Vector[Model] ): Gen[Map[ModelVersionId, ExportedSolutions]] =
    models
      .traverse: model =>
        exportedSolutionsFor( model ).tupleLeft( model.version.version )
      .map( _.toMap )

  def loadModelWithoutFicsmas( version: ModelVersion ): IO[Model] =
    assets
      .loadModel[IO]( version )
      .map( model => ModelConsistency( model, ( _, ex ) => ex != ExtractorType.FicsmasTree ).getOrElse( model ) )

  def generateExportedSolutions( random: Random[IO] ): IO[ExportedSolutions.ByModelVersion] =
    for
      modelIndex <- assets.loadModelIndex[IO]
      models     <- modelIndex.versions.toVector.traverse( loadModelWithoutFicsmas )
      seed       <- random.nextLong
      result     <- exportedSolutions( models )
                  .doPureApply( Gen.Parameters.default, Seed( seed ) )
                  .retrieve
                  .liftTo[IO]( new NoSuchElementException( "generator failed" ) )
    yield ExportedSolutions.ByModelVersion( seed, result )

  def writeExportedSolutions( exportedSolutions: ExportedSolutions.ByModelVersion )( using
      files: Files[IO]
  ): IO[Unit] =
    val directory = Path( "." ) / "testkit" / "src" / "test" / "resources"
    val fileName  = "solutions.json"
    files.createDirectories( directory ) *>
      Stream
        .emit[IO, ExportedSolutions.ByModelVersion]( exportedSolutions )
        .map( _.asJson.spaces2 )
        .through( utf8.encode )
        .through( files.writeAll( directory / fileName ) )
        .compile
        .drain

  override def run( args: List[String] ): IO[ExitCode] =
    for
      random    <- Random.scalaUtilRandom[IO]
      solutions <- generateExportedSolutions( random )
      _         <- writeExportedSolutions( solutions )
    yield ExitCode.Success
