import java.time.Instant
import java.time.temporal.ChronoUnit
import sbtcrossproject.CrossProject

ThisBuild / organization := "net.chwthewke"

// Scala 3 seems allergic
//ThisBuild / conflictManager                        := ConflictManager.strict
//ThisBuild / updateSbtClassifiers / conflictManager := ConflictManager.default

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" ).withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.stnu" )

ThisBuild / Compile / doc / sources                := Seq.empty
ThisBuild / Compile / packageDoc / publishArtifact := false

enablePlugins( Scalafmt )
enablePlugins( Dependencies )

val sharedSettings = Seq(
  scalaVersion := "3.6.4"
)

val aggregateSettings = Seq(
  publish      := {},
  publishLocal := {}
)

val `stnu-core-cross`: CrossProject =
  crossProject( JSPlatform, JVMPlatform )
    .crossType( CrossType.Pure )
    .settings( sharedSettings )
    .settings( name := "stnu-core" )
    .settings(
      cats,
      alleycats,
      kittens,
      mouse,
      catsTime,
      catsParse,
      algebra,
      circe
    )
    .in( file( "core" ) )
    .enablePlugins( Scalac )

val `stnu-core-jvm`: Project = `stnu-core-cross`.jvm
val `stnu-core-js`: Project  = `stnu-core-cross`.js

val `stnu-core`: Project =
  project
    .in( file( "core/target" ) )
    .settings( sharedSettings )
    .settings( aggregateSettings )
    .aggregate( `stnu-core-jvm`, `stnu-core-js` )

val `stnu-tools`: Project = project
  .in( file( "tools" ) )
  .enablePlugins( Scalac )
  .settings( sharedSettings )
  .dependsOn( `stnu-core-jvm` )
  .settings(
    catsParse,
    catsEffect,
    circeParser,
    fs2Core,
    fs2IO,
    fs2DataCirce,
    pureconfig,
    pureconfigCatsEffect,
    pureconfigFs2
  )

val `stnu-assets`: Project = project
  .in( file( "assets" ) )
  .enablePlugins( Scalac )
  .settings( sharedSettings )
  .dependsOn( `stnu-core-jvm` )
  .settings( catsEffect, fs2Core, fs2IO, fs2DataCirce )

val `stnu-protocol-cross`: CrossProject =
  crossProject( JSPlatform, JVMPlatform )
    .crossType( CrossType.Pure )
    .in( file( "protocol" ) )
    .enablePlugins( Scalac )
    .settings( sharedSettings )
    .settings( name := "stnu-protocol" )
    .dependsOn( `stnu-core-cross` )

val `stnu-protocol-jvm`: Project = `stnu-protocol-cross`.jvm
val `stnu-protocol-js`: Project  = `stnu-protocol-cross`.js

val `stnu-protocol`: Project =
  project
    .in( file( "protocol/target" ) )
    .settings( sharedSettings )
    .settings( aggregateSettings )
    .aggregate( `stnu-protocol-jvm`, `stnu-protocol-js` )

val `stnu-backend`: Project = project
  .in( file( "backend" ) )
  .enablePlugins( Scalac )
  .enablePlugins( BuildInfo )
  .settings( buildInfoPackage := "net.chwthewke.stnu.server" )
  .settings( sharedSettings )
  .dependsOn( `stnu-assets`, `stnu-protocol-jvm` )
  .settings(
    http4sCore,
    http4sDsl,
    http4sEmberServer,
    http4sCirce,
    scalatags,
    http4sScalatags,
    pureconfig,
    pureconfigCatsEffect,
    pureconfigFs2,
    pureconfigIp4s,
    pureconfigHttp4s,
    logging
  )

val backendRunnerSettings: Seq[Def.Setting[_]] = Seq(
  Compile / mainClass  := Some( "net.chwthewke.stnu.server.Main" ),
  Compile / run / fork := true
)

// NOTE this module is intended for running the backend from sbt or IntelliJ
//  it could have specific application.conf/logback.xml/assets etc.,
//  matching the requirements for stnu-frontend-run
val `stnu-backend-run`: Project = project
  .in( file( "backend-run" ) )
  .enablePlugins( Scalac )
  .settings( sharedSettings )
  .settings( backendRunnerSettings )
  .dependsOn( `stnu-backend` )

val `stnu-frontend`: Project = project
  .in( file( "frontend" ) )
  .enablePlugins( Scalac )
  .enablePlugins( ScalaJSPlugin )
  .enablePlugins( BuildInfo )
  .settings( buildInfoPackage := "net.chwthewke.stnu.spa" )
  .settings( sharedSettings )
  .settings( scalaJSLinkerConfig ~= { _.withModuleKind( ModuleKind.ESModule ) } )
  .settings( tyrian, http4sCore, http4sDom, http4sCirce )
  .dependsOn( `stnu-protocol-js` )

// NOTE this module is intended for running the frontend from sbt or a terminal
//  with a hot-reload capable dev webserver (via npm scripts using parcel)
val `stnu-frontend-run`: Project = project
  .in( file( "frontend-run" ) )
  .enablePlugins( Scalac )
  .enablePlugins( ScalaJSPlugin )
  .settings( sharedSettings )
  .settings( scalaJSLinkerConfig ~= { _.withModuleKind( ModuleKind.ESModule ) } )
  .enablePlugins( FrontendDev )
  .dependsOn( `stnu-frontend` )

val `stnu-backend-app`: Project =
  project
    .in( file( "backend-app" ) )
    .enablePlugins( Scalac )
    .enablePlugins( JavaServerAppPackaging )
    .enablePlugins( LauncherJarPlugin )
    .settings( sharedSettings )
    .settings( backendRunnerSettings )
    .settings( Packaging.settings( frontendProject = `stnu-frontend` ) )
    .dependsOn( `stnu-backend` )

val `stnu-laws`: Project =
  project
    .in( file( "laws" ) )
    .enablePlugins( Scalac )
    .settings( sharedSettings )
    .settings( munitLaws )
    .dependsOn( `stnu-core-jvm` )

val `stnu-tests`: Project =
  project
    .in( file( "tests" ) )
    .enablePlugins( Scalac )
    .settings( sharedSettings )
    .settings( munitScalacheck )
    .dependsOn(
      `stnu-core-jvm`,
      `stnu-tools`,
      `stnu-assets`,
      `stnu-backend`
    )

val `stnu-jvm`: Project =
  project
    .in( file( "target/stnu-jvm" ) )
    .settings( sharedSettings )
    .settings( aggregateSettings )
    .aggregate(
      `stnu-core-jvm`,
      `stnu-protocol-jvm`,
      `stnu-tools`,
      `stnu-assets`,
      `stnu-backend`,
      `stnu-laws`,
      `stnu-tests`
    )

val stnu: Project =
  project
    .in( file( "." ) )
    .settings( sharedSettings )
    .settings( aggregateSettings )
    .aggregate(
      `stnu-core`,
      `stnu-tools`,
      `stnu-assets`,
      `stnu-protocol`,
      `stnu-backend`,
      `stnu-frontend`,
      `stnu-laws`,
      `stnu-tests`
    )
