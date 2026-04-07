package net.chwthewke.stnu

import fs2.io.file.Path
import java.nio.file.Paths

enum DataVersionStorage(
    val gameSource: Path,
    val textureSource: Path,
    val modelVersion: ModelVersion
):
  case Release1_1
      extends DataVersionStorage(
        DataVersionStorage.steamPath,
        Path.fromNioPath( Paths.get( sys.props( "user.home" ) ) ) /
          "Downloads" / "FModel" / "Output" / "Exports" / "FactoryGame" / "Content",
        ModelVersion( ModelVersionId( 7 ), "Satisfactory 1.1", "r1.1" )
      )

  val docsFile: String = "en-US.json"

object DataVersionStorage
    extends CustomEnum[DataVersionStorage]
    with CatsEnum[DataVersionStorage]
    with OrderEnum[DataVersionStorage]:
  override def keyOf( version: DataVersionStorage ): String = version.docsKey

  val resourcesBase: Path = Path( "." ) / "assets" / "src" / "main" / "resources"

  extension ( version: DataVersionStorage )
    def resourcesDir: Path = resourcesBase / version.docsKey
    def docsKey: String    = version.modelVersion.key

  def epicPath: Path             = Path( "E:\\EpicGames\\Satisfactory" )
  def epicExperimentalPath: Path = Path( "E:\\EpicGames\\SatisfactoryExperimental" )
  def steamPath: Path            = Path( "D:\\SteamLibrary\\steamapps\\common\\Satisfactory" )
