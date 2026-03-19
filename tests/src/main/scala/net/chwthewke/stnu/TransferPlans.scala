package net.chwthewke.stnu

import cats.data.OptionT
import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.*

import persistence.FsPlans
import persistence.Plans
import persistence.PlansPersistenceApi
import server.AppConfig

object TransferPlans extends TransferPlans[IO] with IOApp:

  override def run( args: List[String] ): IO[ExitCode] =
    transferPlans( args ).as( ExitCode.Success )

class TransferPlans[F[_]: Async]:
  given files: Files[F] = Files.forAsync[F]

  def validateTarget( args: List[String] ): F[Path] =
    OptionT
      .when( args.length == 1 )( Path( args.head ) )
      .semiflatTap( files.createDirectories )
      .getOrRaise( new IllegalArgumentException( "missing argument (target directory)" ) )

  def resource( args: List[String] ): Resource[F, Copy[F]] =
    for
      config    <- Resource.eval( ConfigSource.default.loadF[F, AppConfig]() )
      xa        <- persistence.Resources.managedTransactor( config.database )
      targetDir <- Resource.eval( validateTarget( args ) )
      source = Plans( xa )
      target <- Resource.eval( FsPlans.init[F]( targetDir ) )
    yield new Copy( source, target )

  def transferPlans( args: List[String] ): F[Unit] =
    resource( args ).use( _.copyPlans )

private class Copy[F[_]: Async]( private val from: PlansPersistenceApi[F], private val to: PlansPersistenceApi[F] ):
  def copyPlans: F[Unit] =
    from.readPlans
      .flatMap:
        _.traverseVoid: ps =>
          from.readPlan( ps.planId ).foreachF( plan => to.savePlan( plan, ps.updated, overwrite = false ).void )
