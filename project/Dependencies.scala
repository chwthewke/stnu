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
    val circe: Deps          = libraryDependencies += "io.circe" %%% "circe-core"   % circeVersion
    val circeParser: Deps    = libraryDependencies += "io.circe" %%% "circe-parser" % circeVersion

    val enumeratum: Deps = libraryDependencies ++= Seq(
      "com.beachape" %%% "enumeratum"      % "1.7.6",
      "com.beachape" %%% "enumeratum-cats" % "1.7.5"
    )
    val enumeratumCirce: Deps = libraryDependencies += "com.beachape" %%% "enumeratum-circe" % "1.7.5"

    val fs2Core: Deps      = libraryDependencies += "co.fs2"    %%% "fs2-core"            % "3.12.0"
    val fs2IO: Deps        = libraryDependencies += "co.fs2"    %%% "fs2-io"              % "3.12.0"
    val fs2DataCirce: Deps = libraryDependencies += "org.gnieh" %%% "fs2-data-json-circe" % "1.11.2"

    val pureconfigVersion = "0.17.9"
    val pureconfig: Deps = libraryDependencies ++= Seq(
      "com.github.pureconfig" %%% "pureconfig-core"           % pureconfigVersion,
      "com.github.pureconfig" %%% "pureconfig-cats"           % pureconfigVersion,
      "com.github.pureconfig" %%% "pureconfig-generic-scala3" % pureconfigVersion
    )

    val pureconfigEnumeratum: Deps =
      libraryDependencies += "com.github.pureconfig" %%% "pureconfig-enumeratum" % pureconfigVersion
    val pureconfigCatsEffect: Deps =
      libraryDependencies += "com.github.pureconfig" %%% "pureconfig-cats-effect" % pureconfigVersion
    val pureconfigFs2: Deps = libraryDependencies += "com.github.pureconfig" %%% "pureconfig-fs2" % pureconfigVersion
    val pureconfigIp4s: Deps =
      libraryDependencies += "com.github.pureconfig" %%% "pureconfig-ip4s" % pureconfigVersion
    val pureconfigHttp4s: Deps =
      libraryDependencies += "com.github.pureconfig" %%% "pureconfig-http4s" % pureconfigVersion

    val http4sVersion           = "0.23.30"
    val http4sCore: Deps        = libraryDependencies += "org.http4s" %%% "http4s-core"         % http4sVersion
    val http4sDsl: Deps         = libraryDependencies += "org.http4s" %%% "http4s-dsl"          % http4sVersion
    val http4sCirce: Deps       = libraryDependencies += "org.http4s" %%% "http4s-circe"        % http4sVersion
    val http4sClientCore: Deps  = libraryDependencies += "org.http4s" %%% "http4s-client"       % http4sVersion
    val http4sServerCore: Deps  = libraryDependencies += "org.http4s" %%% "http4s-server"       % http4sVersion
    val http4sEmberServer: Deps = libraryDependencies += "org.http4s" %%% "http4s-ember-server" % http4sVersion
    val http4sEmberClient: Deps = libraryDependencies += "org.http4s" %%% "http4s-ember-client" % http4sVersion

    val scalatags: Deps       = libraryDependencies += "com.lihaoyi" %%% "scalatags"        % "0.13.1"
    val http4sScalatags: Deps = libraryDependencies += "org.http4s"  %%% "http4s-scalatags" % "0.25.2"

    val logging: Deps = libraryDependencies ++= Seq(
      "org.slf4j"      % "slf4j-api"       % "2.0.17",
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.typelevel" %% "log4cats-slf4j"  % "2.7.0"
    )

    val ojAlgo: Deps = libraryDependencies += "org.ojalgo" % "ojalgo" % "55.2.0"

    val tyrian: Deps    = libraryDependencies += "io.indigoengine" %%% "tyrian-io"  % "0.13.0"
    val http4sDom: Deps = libraryDependencies += "org.http4s"      %%% "http4s-dom" % "0.2.8"

    val munitScalacheck: Deps = libraryDependencies ++= Seq(
      "org.scalameta"     %%% "munit"             % "1.1.0",
      "org.scalameta"     %%% "munit-scalacheck"  % "1.1.0",
      "org.typelevel"     %%% "munit-cats-effect" % "2.1.0",
      "org.scalacheck"    %%% "scalacheck"        % "1.18.1",
      "io.chrisdavenport" %%% "cats-scalacheck"   % "0.3.2"
    )

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
