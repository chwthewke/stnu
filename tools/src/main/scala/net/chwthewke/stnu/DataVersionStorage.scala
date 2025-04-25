package net.chwthewke.stnu

import fs2.io.file.Path

enum DataVersionStorage(
    val gameSource: Path,
    val textureSourceSubdir: String,
    val modelVersion: ModelVersion
):
  case Release1_0
      extends DataVersionStorage(
        DataVersionStorage.epicPath,
        "Satisfactory1.0",
        ModelVersion( 6, "Satisfactory 1.0", "r1.0" )
      )

  val docsFile: String = "en-US.json"

object DataVersionStorage
    extends Enum[DataVersionStorage]
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
