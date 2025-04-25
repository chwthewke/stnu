package net.chwthewke.stnu
package ingest

import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import io.circe.Decoder
import io.circe.Json
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.*

import game.GameData
import game.MapConfig
import game.ModelInit
import model.Model

class Loader[F[_]: Async]( val version: DataVersionStorage )(
    private val docsCell: AtomicCell[F, Option[Json]],
    private val gameDataCell: AtomicCell[F, Option[GameData]],
    private val modelCell: AtomicCell[F, Option[Model]],
    private val mapConfigCell: AtomicCell[F, Option[MapConfig]]
):
  private def getCache[A]( cell: AtomicCell[F, Option[A]] )( read: F[A] ): F[A] =
    cell.evalModify:
      case Some( a ) => ( a.some, a ).pure[F]
      case None      => read.map( a => ( a.some, a ) )

  def docs: F[Json] =
    getCache( docsCell ):
      readJsonUtf8Bytes[Json]( readClassLoaderResource( s"${version.docsKey}/Docs.json" ) )

  def gameData: F[GameData] =
    getCache( gameDataCell ):
      docs.flatMap( _.as[Vector[GameData]].map( _.combineAll ).liftTo[F] )

  val mapConfig: F[MapConfig] =
    getCache( mapConfigCell ):
      Loader.mapConf( version ).loadF[F, MapConfig]()

  def model: F[Model] =
    getCache( modelCell ):
      ( gameData, mapConfig )
        .flatMapN: ( data, map ) =>
          ModelInit( version.modelVersion, data, map ).leftMap( Error( _ ) ).liftTo[F]

object Loader:
  def apply[F[_]: Async]( version: DataVersionStorage, docs: Option[Json] = none ): Resource[F, Loader[F]] =
    Resource.eval:
      (
        AtomicCell[F].of( docs ),
        AtomicCell[F].of( none[GameData] ),
        AtomicCell[F].of( none[Model] ),
        AtomicCell[F].of( none[MapConfig] )
      ).mapN( new Loader[F]( version )( _, _, _, _ ) )

  private def mapConf( storage: DataVersionStorage ): ConfigSource =
    ConfigSource.resources( s"${storage.docsKey}/map.conf" ).withFallback( ConfigSource.empty )
