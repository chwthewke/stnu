import com.typesafe.sbt.packager.Keys.maintainer
import com.typesafe.sbt.packager.Keys.topLevelDirectory
import org.scalajs.linker.interface.Report
import org.scalajs.sbtplugin.ScalaJSPlugin
import sbt.*
import sbt.Def
import sbt.Keys.*

object Packaging {

  val copyDistJS: TaskKey[Seq[File]] = TaskKey[Seq[File]]( "copyDistJS", "Copy dist js" )

  private def doCopyDistJS( src: Seq[File], dest: File, logger: Logger ): Seq[File] = {
    logger.info( s"Would copy ${src.mkString( ", " )} to $dest" )
    src.map( file => dest / file.name )
  }

  private def copyDistJSTask( frontendProject: ProjectReference ): Def.Initialize[Task[Seq[File]]] = Def.taskDyn {
    Def.task {
      val distJs: Attributed[Report] = ( frontendProject / Compile / ScalaJSPlugin.autoImport.fullLinkJS ).value
      val srcDir: File               = ( frontendProject / Compile / ScalaJSPlugin.autoImport.fullLinkJSOutput ).value
      val distJsFiles: Seq[File] =
        distJs.data.publicModules
          .flatMap( m => Seq( m.jsFileName ) ++ m.sourceMapName )
          .map( f => srcDir / f )
          .toSeq
      val targetDir: File = ( Compile / resourceManaged ).value / "js"
      val logger: Logger  = streams.value.log

      logger.info( s"Copy ${distJsFiles.mkString( ", " )} to $targetDir" )
      IO.copy(
        distJsFiles.map( f => ( f, targetDir / f.name ) ),
        CopyOptions().withOverwrite( true ).withPreserveLastModified( true )
      )
      distJsFiles.map( file => targetDir / file.name )
    }
  }

  def settings( frontendProject: Project ): Seq[Def.Setting[_]] = Seq(
    copyDistJS := copyDistJSTask( frontendProject ).value,
    Compile / resourceGenerators += copyDistJS.taskValue,
    topLevelDirectory := Some( "stnu" ),
    maintainer        := "stnu@chwthewke.net"
  )

}
