import sbtcrossproject.CrossProject

ThisBuild / organization := "net.chwthewke"

// Scala 3 seems allergic
//ThisBuild / conflictManager                        := ConflictManager.strict
//ThisBuild / updateSbtClassifiers / conflictManager := ConflictManager.default

ThisBuild / ideBasePackages.withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.stnu" )

ThisBuild / Compile / doc / sources                := Seq.empty
ThisBuild / Compile / packageDoc / publishArtifact := false

enablePlugins( Scalafmt )
enablePlugins( Dependencies )

val sharedSettings = Seq(
  scalaVersion                                          := "3.6.4",
  ideExcludedDirectories.withRank( KeyRanks.Invisible ) := Seq( target.value )
)

val aggregateSettings = Seq(
  publish      := {},
  publishLocal := {}
)

addCommandAlias( "ci", "; scalafmtAll ; test ; stnu-backend-app / Universal / packageBin" )

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
    .platformsSettings( JSPlatform )( tzdb )
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
    .settings( http4sCore, http4sDsl )
    .dependsOn( `stnu-core-cross` )

val `stnu-protocol-jvm`: Project = `stnu-protocol-cross`.jvm
val `stnu-protocol-js`: Project  = `stnu-protocol-cross`.js

val `stnu-protocol`: Project =
  project
    .in( file( "protocol/target" ) )
    .settings( sharedSettings )
    .settings( aggregateSettings )
    .aggregate( `stnu-protocol-jvm`, `stnu-protocol-js` )

val `stnu-persistence`: Project =
  project
    .in( file( "persistence" ) )
    .settings( sharedSettings )
    .enablePlugins( Scalac )
    .settings( doobie, doobieCirce, flyway, postgresql, pureconfig )
    .dependsOn( `stnu-protocol-jvm` )

val `stnu-backend`: Project = project
  .in( file( "backend" ) )
  .enablePlugins( Scalac )
  .enablePlugins( BuildInfo )
  .settings( buildInfoPackage := "net.chwthewke.stnu.server" )
  .settings( sharedSettings )
  .dependsOn( `stnu-assets`, `stnu-protocol-jvm`, `stnu-persistence` )
  .settings(
    circeParser,
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
    logging,
    ojAlgo
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
  .settings( catsFree, monocle, tyrian, http4sCore, http4sDom, http4sCirce )
  .dependsOn( `stnu-protocol-js` )

// NOTE this module is intended for running the frontend from sbt or a terminal
//  with a hot-reload capable dev webserver (via npm scripts using parcel)
val `stnu-frontend-run`: Project = project
  .in( file( "frontend-run" ) )
  .enablePlugins( Scalac )
  .enablePlugins( ScalaJSPlugin )
  .settings( sharedSettings )
  .settings(
    ideExcludedDirectories ++=
      Seq( ".parcel-cache", "dist", "node_modules" ).map( n => baseDirectory.value / n )
  )
  .settings( scalaJSLinkerConfig ~= { _.withModuleKind( ModuleKind.ESModule ) } )
  .settings( circeParser )
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

val `stnu-testkit-cross`: CrossProject =
  crossProject( JSPlatform, JVMPlatform )
    .crossType( CrossType.Pure )
    .in( file( "testkit" ) )
    .enablePlugins( Scalac )
    .settings( sharedSettings )
    .settings( name := "stnu-testkit" )
    .settings( scalacheck )
    .dependsOn( `stnu-protocol-cross` )

val `stnu-testkit-jvm`: Project = `stnu-testkit-cross`.jvm
val `stnu-testkit-js`: Project  = `stnu-testkit-cross`.js

val `stnu-testkit`: Project =
  project
    .in( file( "testkit/target" ) )
    .settings( sharedSettings )
    .settings( aggregateSettings )
    .aggregate( `stnu-testkit-jvm`, `stnu-testkit-js` )

val `stnu-tests`: Project =
  project
    .in( file( "tests" ) )
    .enablePlugins( Scalac )
    .settings( sharedSettings )
    .settings( munitScalacheck, doobieMunit )
    .settings( testFrameworks += new TestFramework( "munit.Framework" ) )
    .dependsOn(
      `stnu-core-jvm`,
      `stnu-tools`,
      `stnu-assets`,
      `stnu-backend`,
      `stnu-testkit-jvm`
    )

val `stnu-js-tests`: Project =
  project
    .in( file( "tests-scalajs" ) )
    .enablePlugins( Scalac )
    .enablePlugins( ScalaJSPlugin )
    .settings( sharedSettings )
    .settings( munitScalacheck, circeParser )
    .settings( testFrameworks += new TestFramework( "munit.Framework" ) )
    .dependsOn( `stnu-frontend`, `stnu-testkit-js` )

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
      `stnu-persistence`,
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
      `stnu-persistence`,
      `stnu-backend`,
      `stnu-backend-app`,
      `stnu-backend-run`,
      `stnu-frontend`,
      `stnu-frontend-run`,
      `stnu-laws`,
      `stnu-tests`,
      `stnu-js-tests`
    )
