package net.chwthewke.stnu
package persistence

import cats.effect.Async
import cats.effect.Resource
import cats.syntax.all.*
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import doobie.*
import doobie.hikari.HikariTransactor
import javax.sql.DataSource
import org.flywaydb.core.Flyway

object Resources:
  private def hikariConfig( config: Config ): HikariConfig =
    val c = new HikariConfig()
    c.setDataSourceClassName( "org.postgresql.ds.PGSimpleDataSource" )
    c.addDataSourceProperty( "serverName", "localhost" )
    c.addDataSourceProperty( "portNumber", 5432 )
    c.addDataSourceProperty( "databaseName", config.databaseName )
    c.addDataSourceProperty( "user", config.user )
    c.addDataSourceProperty( "password", config.password )
    c

  private def dataSource[F[_]: Async]( config: Config ): Resource[F, HikariDataSource] =
    Resource.fromAutoCloseable( Async[F].delay( new HikariDataSource( hikariConfig( config ) ) ) )

  def transactor[F[_]: Async]( config: Config ): Resource[F, ( DataSource, Transactor[F] )] =
    for
      ec <- ExecutionContexts.fixedThreadPool( 4 )
      ds <- dataSource( config )
    yield ( ds, HikariTransactor[F]( ds, ec ) )

  private def flywayMigrate[F[_]]( ds: DataSource )( using F: Async[F] ): F[Unit] =
    F
      .delay:
        Flyway
          .configure()
          .dataSource( ds )
          .locations( "classpath:net/chwthewke/stnu/persistence" )
          .load()
          .migrate()
      .void

  def managedTransactor[F[_]: Async]( config: Config ): Resource[F, Transactor[F]] =
    transactor[F]( config ).evalMap:
      case ( ds, xa ) => flywayMigrate( ds ).as( xa )
