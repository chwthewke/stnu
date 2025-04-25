package net.chwthewke.stnu

import cats.effect.Sync
import fs2.Stream
import fs2.data.json
import fs2.data.json.circe.*
import fs2.data.json.codec
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import scala.annotation.unused

package object ingest:
  def writeJson[F[_]: Sync, A: Encoder]( value: A, path: Path )( using Files: Files[F] ): F[Unit] =
    Stream
      .emit[F, String]( value.asJson.spaces2SortKeys ) // note fs2.data.json is very slow here
      .through( Files.writeUtf8( path ) )
      .compile
      .drain

  class ReadJsonStringPartiallyApplied[A]( @unused private val dummy: Boolean = false ) extends AnyVal:
    def apply[F[_]: Sync]( stream: Stream[F, String] )( using Decoder[A] ): F[A] =
      stream
        .through( json.tokens )
        .through( codec.deserialize[F, A] )
        .compile
        .lastOrError

  class ReadJsonUtf8BytesPartiallyApplied[A]( @unused private val dummy: Boolean = false ) extends AnyVal:
    def apply[F[_]: Sync]( stream: Stream[F, Byte] )( using Decoder[A] ): F[A] =
      readJsonString[A]( stream.through( text.utf8.decode[F] ) )

  def readJsonString[A: Decoder]: ReadJsonStringPartiallyApplied[A] = new ReadJsonStringPartiallyApplied[A]()

  def readJsonUtf8Bytes[A: Decoder]: ReadJsonUtf8BytesPartiallyApplied[A] = new ReadJsonUtf8BytesPartiallyApplied[A]()

  def readJson[F[_]: Sync, A: Decoder]( path: Path )( using Files: Files[F] ): F[A] =
    readJsonUtf8Bytes[A]( Files.readAll( path ) )
