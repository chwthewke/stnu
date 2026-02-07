package net.chwthewke.stnu
package persistence

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.munit.analysisspec.IOChecker
import _root_.munit.AnyFixture
import _root_.munit.CatsEffectSuite
import _root_.munit.catseffect.IOFixture
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.*

trait TransactorFixture extends CatsEffectSuite:
  val truncate: ConnectionIO[Unit] =
    // language=SQL
    sql"""TRUNCATE TABLE "plans"
         |  RESTART IDENTITY
         |  CASCADE 
         |""".stripMargin.update.run.void

  val transactorResource: Resource[IO, Transactor[IO]] =
    for {
      config <- Resource.eval( ConfigSource.default.at( "database" ).loadF[IO, Config]() )
      xa     <- Resources.managedTransactor[IO]( config )
      _      <- Resource.onFinalize( truncate.transact( xa ) )
    } yield xa

  val transactorFixture: IOFixture[Transactor[IO]] = ResourceSuiteLocalFixture( "transactor", transactorResource )

  def transactor: Transactor[IO] = transactorFixture()

  override def munitFixtures: Seq[AnyFixture[?]] = super.munitFixtures :+ transactorFixture

abstract class CheckTests extends CatsEffectSuite with IOChecker with TransactorFixture
