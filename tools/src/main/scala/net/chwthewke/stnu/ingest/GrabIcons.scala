package net.chwthewke.stnu
package ingest

import cats.data.OptionT
import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.Stream
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import fs2.io.file.Files
import fs2.io.file.Path
import java.nio.file.Paths
import scodec.bits.ByteVector

import data.ImageName
import game.GameData
import game.IconData
import model.IconIndex
import model.Model

class GrabIcons[F[_]: Async]( private val loader: Loader[F] )( using Files: Files[F], Hashing: Hashing[F] ):
  val console: Console[F] = Console.make[F]

  private def findTexture( iconData: IconData ): OptionT[F, TextureData] =
    val targetPath: Path =
      GrabIcons
        .sourcePath( loader.version )
        .resolve( Path.apply( iconData.dir.stripPrefix( "Game/" ) ) )
        .resolve( s"${iconData.textureName}.png" )
    OptionT
      .whenM( Files.isRegularFile( targetPath ) )(
        hash( Files.readAll( targetPath ) ).map( TextureData( s"${iconData.fullName}", targetPath, _ ) )
      )

  private def hash( bytes: Stream[F, Byte] ): F[ByteVector] =
    bytes.through( Hashing.hash( HashAlgorithm.SHA256 ) ).compile.lastOrError.map( _.bytes.toByteVector )

  private def grabIcon( className: ClassName[Any], iconData: IconData ): OptionT[F, ( ClassName[Any], ImageName )] =
    findTexture( iconData )
      .flatTapNone:
        console.println( show"${iconData.fullName} not found" )
      .semiflatMap: texture =>
        val target: Path = GrabIcons.targetImages / texture.filename
        for
          _ <- console.println( show"${iconData.fullName} found as ${texture.path} (${texture.hash.toHex})" )
          _ <- Files.exists( target ).flatMap( exists => Files.copy( texture.path, target ).whenA( !exists ) )
        yield ( className, ImageName( texture.filename ) )

  private def process( iconData: Vector[( ClassName[Any], IconData )] ): F[Unit] =
    iconData
      .traverseFilter( grabIcon.tupled( _ ).value )
      .flatTap: found =>
        console.println( s"Found ${found.size} out of ${iconData.length} icons" )
      .map( imageNames => IconIndex( imageNames.toMap ) )
      .flatMap: index =>
        writeJson( index, GrabIcons.indexPath( loader.version ) )

  private def buildables(
      gameData: GameData,
      classNames: Vector[ClassName[Any]]
  ): F[Vector[( ClassName[Any], IconData )]] =
    classNames.traverse: cn =>
      gameData
        .getBuildingIcon( cn )
        .liftTo[F]( Error( show"No desc class with icon data found for $cn" ) )
        .tupleLeft( cn )

  private def getConveyorBeltIcons( gameData: GameData, model: Model ): F[Vector[( ClassName[Any], IconData )]] =
    buildables( gameData, model.conveyorBelts.map( _.className ) )

  private def getPipelineIcons( gameData: GameData, model: Model ): F[Vector[( ClassName[Any], IconData )]] =
    buildables( gameData, model.pipelines.map( _.className ) )

  private def getMachineIcons( gameData: GameData, model: Model ): F[Vector[( ClassName[Any], IconData )]] =
    buildables(
      gameData,
      ( model.manufacturingRecipes.map( _.producedIn ) ++
        model.powerRecipes.map( _.producedIn ) ++
        model.extractionRecipes.foldMap( _.recipes.map( _.producedIn ) ) ).map( _.className ).distinct
    )

  private def getItemIcons( gameData: GameData, model: Model ): F[Vector[( ClassName[Any], IconData )]] =
    model.items.keys.toVector.foldMapM: className =>
      gameData.items
        .get( ClassName( className.name ) )
        .liftTo[F]( Error( show"Unknown item in model" ) )
        .map( item => Vector( ( className, item.smallIcon ) ) )

  private def getAllIcons( gameData: GameData, model: Model ): F[Vector[( ClassName[Any], IconData )]] =
    (
      getConveyorBeltIcons( gameData, model ),
      getPipelineIcons( gameData, model ),
      getMachineIcons( gameData, model ),
      getItemIcons( gameData, model )
    ).mapN( _ ++ _ ++ _ ++ _ )

  def run: F[Unit] =
    for
      gameData <- loader.gameData
      model    <- loader.model
      iconData <- getAllIcons( gameData, model )
      _        <- Files.createDirectories( GrabIcons.targetImages )
      _        <- Files.createDirectories( GrabIcons.targetIndexDir( loader.version ) )
      _        <- process( iconData )
    yield ()

object GrabIcons:
  private val exportsRoot: Path = Path.fromNioPath( Paths.get( sys.props( "user.home" ) ) ) / "Downloads" / "Output"
  private def sourcePath( storage: DataVersionStorage ): Path =
    exportsRoot / storage.textureSourceSubdir / "Exports" / "FactoryGame" / "Content"
  private val targetImages: Path                                  = DataVersionStorage.resourcesBase / "img"
  private def targetIndexDir( storage: DataVersionStorage ): Path = storage.resourcesDir
  private def indexPath( storage: DataVersionStorage ): Path      = targetIndexDir( storage ) / "index.json"

  abstract class Program( storage: DataVersionStorage ) extends IOApp:
    override def run( args: List[String] ): IO[ExitCode] =
      Loader[IO]( storage ).use: loader =>
        new GrabIcons[IO]( loader ).run.as( ExitCode.Success )

object GrabIconsR1_0 extends GrabIcons.Program( DataVersionStorage.Release1_0 )
object GrabIconsR1_1 extends GrabIcons.Program( DataVersionStorage.Release1_1 )
