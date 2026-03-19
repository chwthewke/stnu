package net.chwthewke.stnu
package persistence

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

class FsPlans[F[_]: Sync]( private val data: Path )( using Files[F] ) extends PlansPersistenceApi[F]:
  private val planFile: String    = "plan"
  private val summaryFile: String = "summary"
  private val indexDir: String    = ".index"

  private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName( "FS" )

  override def readPlans: F[Vector[PlanSummary]] =
    readPlanIds.flatMap: planIds =>
      planIds.toVector.traverseFilter( id => readPlanSummaryFile( data / id.toString / summaryFile ).value )

  override def savePlan( plan: Plan, at: Instant, overwrite: Boolean ): F[Option[PlanId]] =
    val hash: String = PlanFileName.nameHash( plan.name )
    getPlanId( plan.name ).value
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

  override def readPlan( planId: PlanId ): OptionT[F, Plan] = readPlanFile( data / planId.id.toString / planFile )

  override def deletePlan( planId: PlanId ): F[Boolean] =
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
    OptionT.whenM( FileOps.isRegularFile[F]( path ) )( FileOps.readValue( path, Codecs.planName ) )

  private def readPlanFile( path: Path ): OptionT[F, Plan] =
    OptionT.whenM( FileOps.isRegularFile[F]( path ) )( FileOps.readValue( path, Codecs.plan ) )

  private def readPlanSummaryFile( path: Path ): OptionT[F, PlanSummary] =
    OptionT.whenM( FileOps.isRegularFile[F]( path ) )( FileOps.readValue( path, Codecs.planSummary ) )

  private def writePlanFiles( planId: PlanId, plan: Plan, at: Instant ): F[Unit] =
    (
      FileOps.lock[F]( data / planId.id.toString / planFile ),
      FileOps.lock[F]( data / planId.id.toString / summaryFile )
    ).tupled
      .use:
        case ( planHandle, summaryHandle ) =>
          FileOps.writeValue( summaryHandle, Codecs.planSummary )(
            PlanSummary( planId, plan.name, at, plan.requestSelection.requestedAmounts.toVector )
          )
            *> FileOps.writeValue( planHandle, Codecs.plan )( plan )

object FsPlans:
  def init[F[_]: Async]( dataDir: Path ): F[FsPlans[F]] =
    given files: Files[F] = Files.forAsync[F]
    files.createDirectories( dataDir ).as( new FsPlans[F]( dataDir ) )
