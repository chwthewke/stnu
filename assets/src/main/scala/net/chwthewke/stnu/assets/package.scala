package net.chwthewke.stnu

import cats.effect.Sync
import fs2.data.json
import fs2.data.json.circe.*
import fs2.data.json.codec
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.Decoder

import model.IconIndex
import model.Model
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

  def loadModel[F[_]: Sync]( version: ModelVersion ): F[Model] = readJson( s"${version.key}/model.json" )

  def loadIconIndex[F[_]: Sync]( version: ModelVersion ): F[IconIndex] = readJson( s"${version.key}/index.json" )
