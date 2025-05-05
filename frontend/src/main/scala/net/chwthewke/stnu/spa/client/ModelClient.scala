package net.chwthewke.stnu
package spa.client

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.Async
import org.http4s.client.Client

import model.ModelIndex
import protocol.game.FullModel
import protocol.game.ModelApi

class ModelClient[F[_]: Async] extends ModelApi[[a] =>> Kleisli[F, Client[F], a]] with CirceClient[F]:
  override def getModelIndex: Kleisli[F, Client[F], ModelIndex] =
    expect[ModelIndex]( ModelApi.getModelIndex() )

  override def getModel( version: ModelVersionId ): OptionT[[a] =>> Kleisli[F, Client[F], a], FullModel] =
    OptionT( expectOption[FullModel]( ModelApi.getModel( version ) ) )

  override def getLatestModel: Kleisli[F, Client[F], FullModel] =
    expect[FullModel]( ModelApi.getLatestModel() )
