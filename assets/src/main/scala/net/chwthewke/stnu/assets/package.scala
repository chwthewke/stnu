package net.chwthewke.stnu

import cats.effect.Sync
import cats.syntax.all.*
import fs2.Stream
import fs2.data.json
import fs2.data.json.circe.*
import fs2.data.json.codec
import fs2.hashing.HashAlgorithm
import fs2.hashing.Hashing
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.Decoder
import scodec.bits.ByteVector

import model.IconIndex
import model.Model
import model.ModelConsistency
import model.ModelIndex

package object assets:
  private def readJson[F[_]: Sync, A: Decoder]( resource: String ): F[A] =
    readClassLoaderResource( resource )
      .through( text.utf8.decode )
      .through( json.tokens )
      .through( codec.deserialize[F, A] )
      .compile
      .lastOrError

  def loadModelIndex[F[_]: Sync]: F[ModelIndex] = readJson( "index.json" )

  private def resource[F[_]: Sync]( version: ModelVersion ) = s"${version.key}/model.json"

  def loadModel[F[_]: Sync]( version: ModelVersion ): F[Model] =
    readJson[F, Model]( resource( version ) )
      .flatMap: model =>
        ModelConsistency
          .apply( model )
          .leftMap( err => new IllegalStateException( s"Could not enforce model consistency: $err" ) )
          .liftTo[F]

  def loadModelHash[F[_]: Sync]: F[String] =
    Stream
      .evals( loadModelIndex[F].map( _.versions ) )
      .flatMap( version => readClassLoaderResource( resource( version ) ) )
      .through( Hashing.forSync[F].hash( HashAlgorithm.SHA256 ) )
      .flatMap( h => Stream.chunk( h.bytes ) )
      .compile
      .to( ByteVector )
      .map( _.toHex )

  def loadIconIndex[F[_]: Sync]( version: ModelVersion ): F[IconIndex] = readJson( s"${version.key}/index.json" )
