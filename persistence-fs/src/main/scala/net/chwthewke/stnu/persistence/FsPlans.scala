package net.chwthewke.stnu
package persistence

import cats.Monad
import cats.data.NonEmptySet
import cats.data.OptionT
import cats.effect.Async
import cats.effect.Sync
import cats.syntax.all.*
import fs2.io.file.FileAlreadyExistsException
import fs2.io.file.Files
import fs2.io.file.Path
import java.time.Instant
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.collection.immutable.SortedSet

import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanName
import protocol.persistence.PlanSummary

class CurrentFsPlans[F[_]: Sync]( dataDir: Path, codecs: Codecs.Aux[Plan, PlanSummary] )( using Files[F] )
    extends FsPlans[F, Plan, PlanSummary]( dataDir, codecs )
    with PlansPersistenceApi[F]

abstract class FsPlans[F[_]: Sync, Plan, PlanSummary](
    private val data: Path,
    private val codecs: Codecs.Aux[Plan, PlanSummary]
)( using Files[F] ):

  import FsPlans.*

  private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "FS" )

  def readPlans: F[Vector[PlanSummary]] =
    readPlanIds.flatMap: planIds =>
      planIds.toVector.traverseFilter( id => readPlanSummary( PlanId( id ) ).value )

  def savePlan( plan: Plan, at: Instant, overwrite: Boolean ): F[Option[PlanId]] =
    val planName: PlanName = codecs.getPlanName( plan )
    val hash: String       = PlanFileName.nameHash( planName )
    getPlanId( planName ).value
      .flatMap:
        case Some( planId ) =>
          OptionT.whenF( overwrite )( writePlanFiles( planId, plan, at ) ).as( planId ).value
        case None =>
          for
            planId <- createPlanId
            _      <- FileOps.createDirectories( data / indexDir / hash )
            _      <- FileOps.touch( data / indexDir / hash / planId.id.toString )
            _      <- writePlanFiles( planId, plan, at )
          yield planId.some

  def readPlan( planId: PlanId ): OptionT[F, Plan] = readPlanFile( data / planId.id.toString / planFile )

  def deletePlan( planId: PlanId ): F[Boolean] =
    readPlanName( data / planId.id.toString / planFile )
      .semiflatMap: name =>
        FileOps.deleteDirectory( data / indexDir / PlanFileName.nameHash( name ) / planId.id.toString, force = false )
          *> FileOps.deleteDirectory( data / indexDir / PlanFileName.nameHash( name ), force = false )
          *> FileOps.deleteDirectory( data / planId.id.toString, force = true )
      .value
      .map( _.isDefined )

  private def readPlanIds: F[SortedSet[Int]] =
    FileOps
      .listDirs( data )
      .map( d => d.fileName.toString.toIntOption.to( SortedSet ) )
      .compile
      .foldMonoid

  private def newPlanId: F[PlanId] =
    readPlanIds
      .flatTap( ids => logger.info( s"read plan ids ${ids.mkString_( " " )}" ) )
      .map: ( ids: SortedSet[Int] ) =>
        NonEmptySet.fromSet( ids ).fold( PlanId( 1 ) )( s => PlanId( s.last + 1 ) )
      .flatTap( planId => logger.info( show"new plan id $planId" ) )

  private def createPlanId: F[PlanId] =
    ().tailRecM: _ =>
      newPlanId.flatMap: id =>
        FileOps
          .createDirectory( data / id.id.toString )
          .flatTap( _ => logger.info( s"FS created ${data / id.id.toString}" ) )
          .attemptNarrow[FileAlreadyExistsException]
          .map( _.bimap( _ => (), _ => id ) )

  private def getPlanId( name: PlanName ): OptionT[F, PlanId] =
    val hash: String = PlanFileName.nameHash( name )
    OptionT
      .whenM( FileOps.isDirectory[F]( data / indexDir / hash ) )(
        FileOps
          .listFiles( data / indexDir / hash )
          .mapFilter( _.fileName.toString.toIntOption.map( PlanId( _ ) ) )
          .evalFilter( id => readPlanName( data / id.id.toString / planFile ).exists( _ == name ) )
          .head
          .compile
          .last
      )
      .subflatMap( identity )

  private def readPlanName( path: Path ): OptionT[F, PlanName] =
    OptionT.whenM( FileOps.isRegularFile[F]( path ) )( FileOps.readValue( path, codecs.planName ) )

  private def readPlanFile( path: Path ): OptionT[F, Plan] =
    OptionT.whenM( FileOps.isRegularFile[F]( path ) )( FileOps.readValue( path, codecs.plan ) )

  private def readPlanSummary( planId: PlanId ): OptionT[F, PlanSummary] =
    readPlanSummaryFile( data / planId.id.toString / summaryFile )

  private def readPlanSummaryFile( path: Path ): OptionT[F, PlanSummary] =
    OptionT.whenM( FileOps.isRegularFile[F]( path ) )( FileOps.readValue( path, codecs.planSummary ) )

  private def writePlanFiles( planId: PlanId, plan: Plan, at: Instant ): F[Unit] =
    (
      FileOps.lock[F]( data / planId.id.toString / planFile ),
      FileOps.lock[F]( data / planId.id.toString / summaryFile )
    ).tupled
      .use:
        case ( planHandle, summaryHandle ) =>
          FileOps.writeValue( summaryHandle, codecs.planSummary )(
            codecs.toPlanSummary( planId, plan, at )
          )
            *> FileOps.writeValue( planHandle, codecs.plan )( plan )

object FsPlans:
  private class SchemaOps[F[_]: Sync]( private val data: Path )( using Files[F] ):
    def readSchemaVersion: OptionT[F, SchemaVersion] =
      OptionT
        .whenM( FileOps.isRegularFile[F]( data / schemaVersionFile ) ):
          FileOps.readValue( data / schemaVersionFile, Codecs.schemaVersion )

    def writeSchemaVersion( version: SchemaVersion ): F[Unit] =
      FileOps
        .lock( data / schemaVersionFile )
        .use: handle =>
          FileOps.writeValue( handle, Codecs.schemaVersion )( version )

  private val planFile: String          = "plan"
  private val summaryFile: String       = "summary"
  private val indexDir: String          = ".index"
  private val schemaVersionFile: String = ".version"

  def doMigration[F[_]: Async, P0, S0, P1, S1]( dataDir: Path, migration: Codecs.Migration.Aux[P0, S0, P1, S1] )( using
      Files[F]
  ): F[Unit] =
    for
      sourcePlans <- forVersion[F, P0, S0]( dataDir, migration.fromCodecs )
      planIds     <- sourcePlans.readPlanIds
      destPlans   <- forVersion[F, P1, S1]( dataDir, migration.toCodecs )
      _           <- planIds.traverseVoid( id => migratePlan( PlanId( id ), sourcePlans, destPlans, migration ) )
      _           <- new SchemaOps[F]( dataDir ).writeSchemaVersion( migration.toCodecs.version )
    yield ()

  def migratePlan[F[_]: Monad, P0, S0, P1, S1](
      planId: PlanId,
      sourcePlans: FsPlans[F, P0, S0],
      destPlans: FsPlans[F, P1, S1],
      migration: Codecs.Migration.Aux[P0, S0, P1, S1]
  ): F[Unit] =
    ( sourcePlans.readPlanSummary( planId ), sourcePlans.readPlan( planId ) ).tupled.foreachF:
      case ( summary, plan ) =>
        destPlans.writePlanFiles(
          planId,
          migration.upgradePlan( plan ),
          migration.fromCodecs.getSummaryUpdated( summary )
        )

  def doMigrations[F[_]: Async]( dataDir: Path, upTo: Codecs )( using Files[F] ): F[Unit] =
    val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "FS.MIGRATIONS" )
    for
      currentSchemaVersion <- new SchemaOps[F]( dataDir ).readSchemaVersion.value
      _ <- logger.debug( show"Current schema version is: ${currentSchemaVersion.fold( "NONE" )( _.toString )}" )
      lastMigrationIndex =
        currentSchemaVersion
          .flatMap( v => Codecs.migrations.indexWhere( _.toCodecs.version == v ).some.filter( _ >= 0 ) )
      migrations =
        Codecs.migrations
          .drop( lastMigrationIndex.fold( 0 )( _ + 1 ) )
          .takeWhile( _.toCodecs.version <= upTo.version )
      _ <-
        logger.debug(
          show"Applying ${migrations.size} migrations until version " + migrations.lastOption.map( _.version ).mkString
        )
      _ <- migrations.traverseVoid: m =>
             doMigration( dataDir, m ) *>
               logger.info( show"Applied migration: ${m.describe}" )
      _ <- logger.info( "Applied all migrations." )
    yield ()

  def init[F[_]: Async](
      dataDir: Path,
      codecs: Codecs.Aux[Plan, PlanSummary] = Codecs.latest
  ): F[PlansPersistenceApi[F]] =
    given files: Files[F] = Files.forAsync[F]
    for
      _ <- files.createDirectories( dataDir )
      _ <- doMigrations( dataDir, upTo = codecs )
    yield new CurrentFsPlans[F]( dataDir, codecs )

  private def forVersion[F[_]: Async, P, S]( dataDir: Path, codecs: Codecs.Aux[P, S] ): F[FsPlans[F, P, S]] =
    given files: Files[F] = Files.forAsync[F]
    files.createDirectories( dataDir ).as( new FsPlans[F, P, S]( dataDir, codecs ) {} )
