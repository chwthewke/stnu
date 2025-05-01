import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbt.*
import sbt.Keys.*
import scala.sys.process.Process
import scala.sys.process.BasicIO
import scala.sys.process.ProcessIO
import scala.sys.process.ProcessLogger

object FrontendDev extends AutoPlugin {
  override def requires: Plugins = super.requires && ScalaJSPlugin
  /////////////////////////
  // Generate launch.js

  val genLaunchJs: TaskKey[Seq[File]] = taskKey[Seq[File]]( "Generates launch.js file" )

  def generateLaunchJs( targetDir: File, jsDir: File, logger: Logger ): Seq[File] = {
    val relPath: File      = jsDir.relativeTo( targetDir ).get
    val importFrom: String = s"./${relPath.toString.replace( '\\', '/' )}/main.js"

    val target: File = targetDir / "launch.js"
    val contents =
      s"""// GENERATED FILE, DO NOT EDIT!
         |import { TyrianApp } from "$importFrom";
         |
         |TyrianApp.launch("app", { "backend": "http://localhost:7869" })
         |""".stripMargin

    val noModifications =
      target.isFile && IO.read( target, IO.utf8 ) == contents

    if ( noModifications ) {
      logger.info( s"genLaunchJs up to date in $target" )
    } else {
      IO.write( target, contents, IO.utf8 )
      logger.info( s"genLaunchJs wrote $target" )
    }

    Seq( target )
  }

  def generateLaunchJsTask = Def.task {
    val targetDir: File = target.value
    val jsDir: File     = ( Compile / fastLinkJSOutput ).value
    val logger: Logger  = streams.value.log

    generateLaunchJs( targetDir, jsDir, logger )
  }

  /////////////////////////
  // NPM tasks

  val devBuild: TaskKey[Unit] = taskKey[Unit]( "Builds with parcel" )
  val devStart: TaskKey[Unit] = taskKey[Unit]( "Runs parcel dev server" )
  val devClean: TaskKey[Unit] = taskKey[Unit]( "Cleans parcel staging" )
  val devReset: TaskKey[Unit] = taskKey[Unit]( "Cleans parcel staging and node_modules" )

  def runNpmCommand( parts: Seq[String], moduleDir: File, logger: Logger, streaming: Boolean = false ): Unit = {

    val errLogger: ProcessLogger =
      ProcessLogger( _ => (), err => logger.warn( s"[npm] $err" ) )

    val cmdPrefix: Seq[String] = {
      val osName: String = Option( System.getProperty( "os.name" ) ).map( _.toLowerCase ).getOrElse( "" )
      if ( osName.startsWith( "windows" ) )
        Seq( "cmd.exe", "/C", "npm" )
      else
        Seq( "npm" )
    }

    val ( processIO, getResult ) = if ( streaming ) {
      val processIO: ProcessIO = BasicIO( withIn = false, outln => logger.info( s"[npm] $outln" ), Some( errLogger ) )
      ( processIO, () => None )
    } else {
      val buf: StringBuffer    = new StringBuffer
      val processIO: ProcessIO = BasicIO( withIn = false, buf, Some( errLogger ) )
      ( processIO, () => Some( buf.toString ) )
    }

    val process: Process = Process( cmdPrefix ++ parts, Some( moduleDir ) )
      .run( processIO )

    val exit: Int              = process.exitValue()
    val result: Option[String] = getResult()

    if ( exit != 0 )
      logger.warn( s"'npm ${parts.mkString( " " )}' exited with code $exit" )
    logger.info( s"'npm ${parts.mkString( " " )}' complete${result.map( ":\n" + _ ).getOrElse( "" )}" )
  }

  def npmInstall( moduleDir: File, logger: Logger ): Unit =
    runNpmCommand( Seq( "install" ), moduleDir, logger )

  def npmTask( parts: Seq[String], streaming: Boolean = false ): Def.Initialize[Task[Unit]] = Def.task {
    val _               = genLaunchJs.value
    val moduleDir: File = baseDirectory.value
    val logger: Logger  = streams.value.log

    npmInstall( moduleDir, logger )
    runNpmCommand( parts, moduleDir, logger, streaming )
  }

  def buildTask: Def.Initialize[Task[Unit]] = npmTask( Seq( "run", "build" ) )

  def startTask: Def.Initialize[Task[Unit]] = npmTask( Seq( "run", "start" ), streaming = true )

  def deleteDir( dir: File, logger: Logger ): Unit = {
    IO.delete( dir )
    logger.info( s"Deleted directory $dir" )
  }

  def cleanTask: Def.Initialize[Task[Unit]] = Def.task {
    val moduleDir: File = baseDirectory.value
    val logger: Logger  = streams.value.log

    deleteDir( moduleDir / "dist", logger )
  }

  def resetTask: Def.Initialize[Task[Unit]] = Def.task {
    val _               = cleanTask.value
    val moduleDir: File = baseDirectory.value
    val logger: Logger  = streams.value.log

    deleteDir( moduleDir / "node_modules", logger )
  }

  ////////////////
  // Plugin settings

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    devBuild    := buildTask.value,
    devStart    := startTask.value,
    devClean    := cleanTask.value,
    devReset    := resetTask.value,
    genLaunchJs := generateLaunchJsTask.value,
    clean := {
      val _ = clean.value
      devClean.value
    }
  )

}
