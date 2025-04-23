ThisBuild / organization := "net.chwthewke"
ThisBuild / scalaVersion := "3.6.4"

ThisBuild / conflictManager                        := ConflictManager.strict
ThisBuild / updateSbtClassifiers / conflictManager := ConflictManager.default

ThisBuild / SettingKey[Seq[String]]( "ide-base-packages" ).withRank( KeyRanks.Invisible ) := Seq( "net.chwthewke.stnu" )

ThisBuild / Compile / doc / sources                := Seq.empty
ThisBuild / Compile / packageDoc / publishArtifact := false

enablePlugins( Scalafmt )
