package net.chwthewke.stnu
package ingest

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.Stream
import fs2.hashing.Hashing
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.syntax.*

import model.Model
import model.ModelIndex

class WriteModel[F[_]: {Async, Hashing}]( val version: DataVersionStorage )( using Files: Files[F] ):
  private def run( forceIndex: Boolean )( getModel: F[Model] ): F[Unit] =
    for
      model      <- getModel
      modelIndex <- updatedModelIndex( forceIndex )
      _          <- writeModel( model, modelIndex )
      _          <- writeModelForTests( model, modelIndex )
    yield ()

  private val readModelIndex: F[ModelIndex] =
    Files
      .isRegularFile( WriteModel.modelIndexPath )
      .ifM(
        readJson[F, ModelIndex]( WriteModel.modelIndexPath ),
        ModelIndex.empty.pure[F]
      )

  private def updatedModelIndex( forceIndex: Boolean ): F[Option[ModelIndex]] =
    readModelIndex.map( index =>
      Option.when( forceIndex || !index.versions.contains( version.modelVersion ) )( index.add( version.modelVersion ) )
    )

  private def writeModel( model: Model, modelIndex: Option[ModelIndex] ): F[Unit] =
    writeJson( model, WriteModel.modelPath( version ) ) *>
      modelIndex.traverse_( writeJson( _, WriteModel.modelIndexPath ) )

  private val testModelsSkeleton: String = """package net.chwthewke.stnu
                                             |package assets
                                             |
                                             |import model.ModelIndex
                                             |
                                             |object Models:
                                             |// format: off
                                             |
                                             |end Models
                                             |""".stripMargin

  private def readTestModels: F[Vector[String]] =
    Files
      .isRegularFile( WriteModel.testModelsPath )
      .ifM(
        Files
          .readUtf8Lines( WriteModel.testModelsPath )
          .map( _.stripSuffix( "\n" ).stripSuffix( "\r" ) )
          .compile
          .toVector,
        testModelsSkeleton.linesIterator.toVector.pure
      )

  private val indexLineStart: String = "val index: ModelIndex"

  private def indexLine( modelIndex: ModelIndex ): String =
    modelIndex.versions
      .map( v => s"""ModelVersion( version = ModelVersionId( ${v.version.id} ), "${v.name}", "${v.key}" )""" )
      .mkString( s"  $indexLineStart = ModelIndex( ", " :: ", " :: Nil )" )

  private val modelsLineStart: String = "val models: Map[ModelVersionId, String]"

  private def modelsLine( modelIndex: ModelIndex ): String =
    modelIndex.versions
      .map( v => s"""ModelVersionId( ${v.version.id} ) -> `${v.key}`""" )
      .mkString( s"  $modelsLineStart = Map( ", ", ", ")" )

  private def modelLineStart( model: Model ): String =
    s"val `${model.version.key}`: String"

  private def modelLine( model: Model ): String =
    model.asJson.noSpaces
      .grouped( 64_000 )
      .map( s => s"raw\"\"\"" + s + "\"\"\"" )
      .mkString( s"  ${modelLineStart( model )} = ", " ++ ", "" )

  private def indexOpt( index: Int ): Option[Int] = if ( index >= 0 ) Some( index ) else None

  private def insertLine( lines: Vector[String], replace: Option[Int], before: Int, line: String ): Vector[String] =
    replace match
      case Some( ix ) => lines.patch( ix, Vector( line ), 1 )
      case None       => lines.patch( before, Vector( line, "" ), 0 )

  private def writeModelForTests(
      model: Model,
      modelIndex: Option[ModelIndex]
  ): F[Unit] =
    readTestModels.flatMap: lines =>
      val endLineIx: Int            = lines.indexWhere( _.startsWith( "end Models" ) )
      val indexLineIx: Option[Int]  = indexOpt( lines.indexWhere( _.trim.startsWith( indexLineStart ) ) )
      val modelsLineIx: Option[Int] = indexOpt( lines.indexWhere( _.trim.startsWith( modelsLineStart ) ) )
      val modelLineIx: Option[Int]  = indexOpt( lines.indexWhere( _.trim.startsWith( modelLineStart( model ) ) ) )
      // start from the end, so as not to have to recompute indexes
      val withModelIndex: Vector[String] = modelIndex match
        case None => lines
        case Some( index ) =>
          val withIndex: Vector[String] = insertLine( lines, indexLineIx, endLineIx, indexLine( index ) )
          insertLine( withIndex, modelsLineIx, indexLineIx.getOrElse( endLineIx ), modelsLine( index ) )
      val withModel: Vector[String] =
        insertLine(
          withModelIndex,
          modelLineIx,
          modelsLineIx.orElse( indexLineIx ).getOrElse( endLineIx ),
          modelLine( model )
        )
      Files.createDirectories( WriteModel.testModelsDirectory ) *>
        Stream
          .emits( withModel )
          .map( _ + Files.lineSeparator )
          .covary[F]
          .through( Files.writeUtf8( WriteModel.testModelsPath ) )
          .compile
          .drain

  def runLocal( forceIndex: Boolean ): F[Unit] =
    run( forceIndex ):
      Loader[F]( version ).use( _.model )

  def grabAndRun( forceIndex: Boolean ): F[Unit] =
    run( forceIndex ):
      for
        json <- GrabDocs( version ).run
        model <- Loader[F]( version, json.some ).use: loader =>
                   GrabIcons( loader ).run *> loader.model
      yield model

object WriteModel:
  val modelIndexPath: Path                           = DataVersionStorage.resourcesBase / "index.json"
  def modelPath( version: DataVersionStorage ): Path = version.resourcesDir / "model.json"
  def testModelsDirectory: Path =
    DataVersionStorage.testResourcesBase / "net" / "chwthewke" / "stnu" / "assets"
  def testModelsPath: Path = testModelsDirectory / "Models.scala"

  abstract class Program( storage: DataVersionStorage, method: WriteModel[IO] => Boolean => IO[Unit] ) extends IOApp:
    override def run( args: List[String] ): IO[ExitCode] =
      val forceIndex = args.headOption.exists( a => a == "--force" || a == "-f" )
      method( WriteModel[IO]( storage ) )( forceIndex ).as( ExitCode.Success )
  abstract class LocalProgram( storage: DataVersionStorage ) extends Program( storage, _.runLocal )
  abstract class GrabProgram( storage: DataVersionStorage )  extends Program( storage, _.grabAndRun )

object WriteLocalModel1_0 extends WriteModel.LocalProgram( DataVersionStorage.Release1_0 )
object WriteLocalModel1_1 extends WriteModel.LocalProgram( DataVersionStorage.Release1_1 )

object GrabAndWriteModel1_0 extends WriteModel.GrabProgram( DataVersionStorage.Release1_0 )
object GrabAndWriteModel1_1 extends WriteModel.GrabProgram( DataVersionStorage.Release1_1 )
