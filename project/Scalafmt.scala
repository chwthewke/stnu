import sbt._
import org.scalafmt.sbt.ScalafmtPlugin
import org.scalafmt.sbt.ScalafmtPlugin.autoImport._

object Scalafmt extends AutoPlugin {

  override def requires: Plugins = ScalafmtPlugin

  val scalafmtGenerateConfig: TaskKey[Unit] =
    TaskKey[Unit]( "scalafmtGenerateConfig" )

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtOnCompile := !sys.props.contains( "idea.runid" )
  )

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtGenerateConfig := {
      IO.write(
        file( ".scalafmt.conf" ),
        """version = "3.9.3"
          |runner.dialect = scala3
          |
          |preset = defaultWithAlign
          |maxColumn = 120
          |lineEndings = preserve
          |
          |assumeStandardLibraryStripMargin = true
          |align.arrowEnumeratorGenerator = true
          |docstrings.style = Asterisk
          |spaces.inParentheses = true
          |
          |newlines.beforeCurlyLambdaParams = multilineWithCaseOnly
          |newlines.avoidForSimpleOverflow = [slc]
          |
          |rewrite.rules = [Imports]
          |rewrite.imports.expand = true
          |""".stripMargin
      )
    },
    scalafmtConfig := {
      val _ = scalafmtGenerateConfig.value
      file( ".scalafmt.conf" )
    }
  )
}
