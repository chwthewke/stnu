package net.chwthewke.stnu
package ingest

import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import io.circe.Decoder
import io.circe.Json

import game.GameData

class Loader[F[_]: Async]( val version: DataVersionStorage )(
    private val docsCell: AtomicCell[F, Option[Json]],
    private val gameDataCell: AtomicCell[F, Option[GameData]]
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

object Loader:
  def apply[F[_]: Async]( version: DataVersionStorage, docs: Option[Json] = none ): Resource[F, Loader[F]] =
    Resource.eval:
      (
        AtomicCell[F].of( docs ),
        AtomicCell[F].of( none[GameData] )
      ).mapN( new Loader[F]( version )( _, _ ) )
