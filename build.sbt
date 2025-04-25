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

val sharedSettings = scalaVersion := "3.6.4"

val `stnu-core`: CrossProject =
  crossProject( JSPlatform, JVMPlatform )
    .crossType( CrossType.Pure )
    .settings( sharedSettings )
    .settings( name := "stnu-core" )
    .jsSettings( name := "stnu-core-js" )
    .jvmSettings( name := "stnu-core-jvm" )
    .settings(
      cats,
      alleycats,
      kittens,
      mouse,
      catsParse,
      algebra,
      circe
    )
    .in( file( "core" ) )
    .enablePlugins( Scalac )

lazy val `stnu-core-jvm`: Project = `stnu-core`.jvm
lazy val `stnu-core-js`: Project  = `stnu-core`.js

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
    .dependsOn( `stnu-core-jvm` )

val `stnu-jvm`: Project =
  project
    .in( file( "target/stnuJVM" ) )
    .aggregate( `stnu-core-jvm`, `stnu-laws`, `stnu-tests` )

val stnu: Project =
  project
    .in( file( "." ) )
    .aggregate( `stnu-core-jvm`, `stnu-core-js`, `stnu-laws`, `stnu-tests` )
