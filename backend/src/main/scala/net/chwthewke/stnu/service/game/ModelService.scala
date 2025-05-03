package net.chwthewke.stnu
package service
package game

import cats.Monad
import cats.data.NonEmptyMap
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.Method.GET
import org.http4s.circe.CirceEntityEncoder.*
import scala.collection.immutable.SortedSet

import model.Model
import model.ModelIndex
import protocol.game.ModelApi
import protocol.codec.UriCodec

class ModelService[F[_]: Monad](
    private val index: ModelIndex,
    private val models: NonEmptyMap[ModelVersionId, Model]
) extends ModelApi[F]
    with UriCodec.Dsl[F]:

  def getModelIndex: F[ModelIndex] = index.pure[F]

  def getModel( version: ModelVersionId ): OptionT[F, Model] =
    OptionT.fromOption[F]( models( version ).map( _.masked ) )

  def getLatestModel: F[Model] = models.last._2.masked.pure[F]

  val routes: HttpRoutes[F] =
    import ModelService.*
    HttpRoutes.of:
      case GET -> MA.getModelIndex()  => Ok( getModelIndex )
      case GET -> MA.getModel( id )   => getModel( id ).cataF( NotFound(), Ok( _ ) )
      case GET -> MA.getLatestModel() => Ok( getLatestModel )

object ModelService:
  val MA: ModelApi.type = ModelApi

  def apply[F[_]: Monad]( models: NonEmptyMap[ModelVersionId, Model] ): ModelService[F] =
    new ModelService[F](
      ModelIndex( models.toList.map( _.version ).to( SortedSet ) ),
      models
    )

  def load[F[_]: Sync]: F[ModelService[F]] =
    for
      modelIndex <- assets.loadModelIndex[F]
      models <- modelIndex.versions.toVector.traverse: version =>
                  assets.loadModel[F]( version ).tupleLeft( version.version )
      modelsNev <- models.toNev.liftTo[F]( Error( "No model was loaded - index empty" ) )
    yield new ModelService[F]( modelIndex, modelsNev.toNem )
