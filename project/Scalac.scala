import sbt._
import sbt.Keys._

object Scalac extends AutoPlugin {

  val allScalacOptions: Seq[String] =
    Seq(
      "-encoding",
      "utf8",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Wconf:any:verbose",
      "-Wsafe-init",
      "-Wunused:implicits",
      "-Wunused:explicits",
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:privates",
      "-Wvalue-discard",
      "-Xfatal-warnings",
      "-Xkind-projector",
      "-Yexplicit-nulls"
    )

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      scalacOptions ++= allScalacOptions,
      Compile / console / scalacOptions := ( Compile / console / scalacOptions ).value
        .filterNot( _ == "-Xfatal-warnings" )
    )

}
