import org.portablescala.sbtplatformdeps.PlatformDepsPlugin
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.toPlatformDepsGroupID
import sbt.*
import sbt.Def
import sbt.Keys.*

object Dependencies extends AutoPlugin {
  override def requires: Plugins = super.requires && PlatformDepsPlugin

  object autoImport {
    type Deps = Def.Setting[Seq[ModuleID]]

    val catsVersion: String = "2.13.0"
    val cats: Deps          = libraryDependencies += "org.typelevel" %%% "cats-core"      % catsVersion
    val catsFree: Deps      = libraryDependencies += "org.typelevel" %%% "cats-free"      % catsVersion
    val mouse: Deps         = libraryDependencies += "org.typelevel" %%% "mouse"          % "1.3.2"
    val kittens: Deps       = libraryDependencies += "org.typelevel" %%% "kittens"        % "3.5.0"
    val alleycats: Deps     = libraryDependencies += "org.typelevel" %%% "alleycats-core" % catsVersion
    val catsTime: Deps      = libraryDependencies += "org.typelevel" %%% "cats-time"      % "0.5.1"
    val catsParse: Deps     = libraryDependencies += "org.typelevel" %%% "cats-parse"     % "1.1.0"

    val algebra: Deps = libraryDependencies += "org.typelevel" %%% "algebra" % "2.13.0"

    val catsEffectVersion: String = "3.6.1"
    val catsEffectKernel: Deps    = libraryDependencies += "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion
    val catsEffect: Deps          = libraryDependencies += "org.typelevel" %%% "cats-effect"        % catsEffectVersion

    val circeVersion: String = "0.14.13"
    val circe: Deps          = libraryDependencies += "io.circe" %%% "circe-core" % circeVersion

    val enumeratum: Deps = libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum"      % "1.7.6",
      "com.beachape" %%% "enumeratum-cats" % "1.7.5"
    )
    val enumeratumCirce: Deps = libraryDependencies += "com.beachape" %%% "enumeratum-circe" % "1.7.5"

    // munit-discipline has not been rebuilt against (binary-incompatible) munit 1.1.0 yet
    val munitLaws: Deps = libraryDependencies ++= Seq(
      "org.scalameta"     %%% "munit"            % "1.0.0",
      "org.typelevel"     %%% "discipline-core"  % "1.7.0",
      "org.typelevel"     %%% "discipline-munit" % "2.0.0",
      "org.typelevel"     %%% "cats-laws"        % catsVersion,
      "io.chrisdavenport" %%% "cats-scalacheck"  % "0.3.2"
    )
  }
}
