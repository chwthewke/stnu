import java.time.Instant
import java.time.temporal.ChronoUnit
import sbt._
import sbt.Keys._
import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoKey
import sbtbuildinfo.BuildInfoKeys._

object BuildInfo extends AutoPlugin {

  override def requires: Plugins = super.requires && BuildInfoPlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    buildInfoObject  := "StnuBuildInfo",
    buildInfoPackage := "net.chwthewke.stnu.server",
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      BuildInfoKey.action( "builtAt" )( Instant.now().truncatedTo( ChronoUnit.SECONDS ) )
    )
  )
}
