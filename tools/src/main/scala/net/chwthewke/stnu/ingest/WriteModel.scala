package net.chwthewke.stnu
package ingest

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.hashing.Hashing
import fs2.io.file.Files
import fs2.io.file.Path

import model.Model
import model.ModelIndex

class WriteModel[F[_]: {Async, Hashing}]( val version: DataVersionStorage )( using Files: Files[F] ):
  private def run( getModel: F[Model] ): F[Unit] = getModel.flatMap( writeModel )

  private val readModelIndex: F[ModelIndex] =
    Files
      .isRegularFile( WriteModel.modelIndexPath )
      .ifM(
        readJson[F, ModelIndex]( WriteModel.modelIndexPath ),
        ModelIndex.empty.pure[F]
      )

  private def writeModel( model: Model ): F[Unit] =
    readModelIndex.flatMap: index =>
      writeJson( model, WriteModel.modelPath( version ) ) *>
        writeJson( index.add( version.modelVersion ), WriteModel.modelIndexPath )
          .whenA( !index.versions.contains( version.modelVersion ) )

  def runLocal: F[Unit] =
    run:
      Loader[F]( version ).use( _.model )

  def grabAndRun: F[Unit] =
    run:
      for
        json <- GrabDocs( version ).run
        model <- Loader[F]( version, json.some ).use: loader =>
                   GrabIcons( loader ).run *> loader.model
      yield model

object WriteModel:
  val modelIndexPath: Path                           = DataVersionStorage.resourcesBase / "index.json"
  def modelPath( version: DataVersionStorage ): Path = version.resourcesDir / "model.json"

  abstract class Program( storage: DataVersionStorage, method: WriteModel[IO] => IO[Unit] ) extends IOApp:
    override def run( args: List[String] ): IO[ExitCode] =
      method( WriteModel[IO]( storage ) ).as( ExitCode.Success )
  abstract class LocalProgram( storage: DataVersionStorage ) extends Program( storage, _.runLocal )
  abstract class GrabProgram( storage: DataVersionStorage )  extends Program( storage, _.grabAndRun )

object WriteLocalModel1_0 extends WriteModel.LocalProgram( DataVersionStorage.Release1_0 )

object GrabAndWriteModel1_0 extends WriteModel.GrabProgram( DataVersionStorage.Release1_0 )
