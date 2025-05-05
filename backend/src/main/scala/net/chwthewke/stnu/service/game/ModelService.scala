package net.chwthewke.stnu
package service
package game

import cats.Monad
import cats.data.NonEmptyMap
import cats.data.NonEmptyVector
import cats.data.OptionT
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.Method.GET
import org.http4s.circe.CirceEntityEncoder.*

import model.ModelIndex
import protocol.codec.UriCodec
import protocol.game.FullModel
import protocol.game.ModelApi

class ModelService[F[_]: Monad](
    private val index: ModelIndex,
    private val models: NonEmptyMap[ModelVersionId, FullModel]
) extends ModelApi[F]
    with UriCodec.Dsl[F]:

  def getModelIndex: F[ModelIndex] = index.pure[F]

  def getModel( version: ModelVersionId ): OptionT[F, FullModel] =
    OptionT.fromOption[F]( models( version ) )

  def getLatestModel: F[FullModel] = models.last._2.pure[F]

  val routes: HttpRoutes[F] =
    import ModelService.*
    HttpRoutes.of:
      case GET -> MA.getModelIndex()  => Ok( getModelIndex )
      case GET -> MA.getModel( id )   => getModel( id ).cataF( NotFound(), Ok( _ ) )
      case GET -> MA.getLatestModel() => Ok( getLatestModel )

object ModelService:
  val MA: ModelApi.type = ModelApi

  def apply[F[_]: Monad]( modelIndex: ModelIndex, models: NonEmptyVector[FullModel] ): ModelService[F] =
    new ModelService[F](
      modelIndex,
      models.fproductLeft( _.game.version.version ).toNem
    )
